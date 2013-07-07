defmodule URI do
  @moduledoc """
  Utilities for working with and creating URIs.

  In order to make sure that all URIs that are handled by user code are valid,
  use `URI.parse/1` to convert a properly escaped URI string into a `URI.Info`
  record and `to_binary/1` to convert a `URI.Info` record to a string.
  """

  defrecord Info, [scheme: nil, path: nil, query: nil,
                   fragment: nil, authority: nil,
                   userinfo: nil, host: nil, port: nil]

  @ports [
    { "ftp", 21 },
    { "http", 80 },
    { "https", 443 },
    { "ldap", 389 },
    { "sftp", 22 },
    { "tftp", 69 },
  ]

  Enum.each @ports, fn { scheme, port } ->
    #def normalize_scheme(unquote(scheme)), do: unquote(scheme)
    def default_port(unquote(scheme)),     do: unquote(port)
  end

  @doc """
  Normalizes the scheme according to the spec by downcasing it.
  """
  def normalize_scheme(nil),     do: nil
  def normalize_scheme(scheme),  do: String.downcase(scheme)

  @doc """
  Returns the default port for a given scheme.

  If the scheme is not known to URI, returns nil.

  A new scheme can be registered via `URI.default_port/2`.
  """
  def default_port(scheme) when is_binary(scheme) do
    { :ok, dict } = :application.get_env(:elixir, :uri)
    Dict.get(dict, scheme)
  end

  @doc """
  Registers a default port for `scheme` in the context of the currently running
  Elixir application.
  """
  def default_port(scheme, port) when is_binary(scheme) and port > 0 do
    { :ok, dict } = :application.get_env(:elixir, :uri)
    :application.set_env(:elixir, :uri, Dict.put(dict, scheme, port))
  end

  @doc """
  Takes an enumerable (containing a sequence of two-item tuples)
  and returns a string of k=v&k2=v2... where keys and values are
  URL encoded as per encode. Keys and values can be any term
  that implements the Binary.Chars protocol (i.e. can be converted
  to binary).
  """
  # format is one of:
  # * :form
  def encode_query(l, format), do: Enum.map_join(l, "&", pair(&1))

  @doc """
  Given a query string of the form "key1=value1&key=value2...", produces an
  orddict with one entry for each key-value pair. Each key and value will be a
  binary. It also does percent-unescaping of both keys and values.

  Use decoder/1 if you want to customize or iterate each value manually.
  """
  # format is one of:
  # * :form
  def decode_query(q, format, dict // HashDict.new) when is_binary(q) do
    Enum.reduce query_decoder(q), dict, fn({ k, v }, acc) -> Dict.put(acc, k, v) end
  end

  @doc """
  Returns an iterator function over the query string that decodes
  the query string in steps.
  """
  def query_decoder(q) when is_binary(q) do
    fn(acc, fun) ->
      do_decoder(q, acc, fun)
    end
  end

  defp do_decoder("", acc, _fun) do
    acc
  end

  defp do_decoder(q, acc, fun) do
    next =
      case :binary.split(q, "&") do
        [first, rest] -> rest
        [first]       -> ""
      end

    current =
      case :binary.split(first, "=") do
        [ key, value ] -> { decode(key), decode(value) }
        [ key ]        -> { decode(key), nil }
      end

    do_decoder(next, fun.(current, acc), fun)
  end

  defp pair({k, v}) do
    encode(to_binary(k)) <> "=" <> encode(to_binary(v))
  end

  ###

  defp percent_encode(s) when is_binary(s), do:
    bc <<c>> inbits s, do: <<percent(c) :: binary>>

  # The so called "unreserved" characters need not be escaped
  defp percent_encode(c)
    when (c in ?0..?9)
      or (c in ?a..?z)
      or (c in ?A..?Z)
      or (c in [?-, ?., ?_, ?~]),
    do: <<c>>

  # The rest of the characters are escaped
  defp percent(c), do: "%" <> hex_byte(<<c>>)

  ###

  defp percent_decode(<<?%, hex1, hex2, tail :: binary>>), do:
    << hex2dec(hex1) :: size(4),
       hex2dec(hex2) :: size(4) >> <> percent_decode(tail)

  defp percent_decode(<<head, tail :: binary>>), do:
    <<head>> <> percent_decode(tail)

  defp percent_decode(<<>>), do: <<>>

  ###

  defp hex_byte(<<hi :: size(4), lo :: size(4)>>), do:
    <<dec2hex(hi)>> <> <<dec2hex(lo)>>

  defp dec2hex(n) when n in 0..9,   do: ?0 + n
  defp dec2hex(n) when n in 10..15, do: ?A + n - 10

  defp hex2dec(n) when n in ?0..?9, do: n - ?0
  defp hex2dec(n) when n in ?A..?F, do: n - ?A + 10
  defp hex2dec(n) when n in ?a..?f, do: n - ?a + 10

  @doc """
  Parses a URI into URI.Info record.

  URIs have portions that are handled specially for the
  particular scheme of the URI. For example, http and https
  have different default ports. Sometimes the parsing
  of portions themselves are different. This parser
  is extensible via behavior modules. If you have a
  module named URI.MYSCHEME with a function called
  'parse' that takes a single argument, the generically
  parsed URI, that function will be called when this
  parse function is passed a URI of that scheme. This
  allows you to build on top of what the URI library
  currently offers. You also need to define default_port
  which takes 0 arguments and returns the default port
  for that particular scheme. Take a look at URI.HTTPS for an
  example of one of these extension modules.
  """
  def parse(uri_string) when is_binary(uri_string) do
    { scheme, authority, path, query, fragment }
      = parse_components(uri_string) |> normalize_components()

    { useinfo, host, port } = split_authority(scheme, authority)

    if nil?(port) and not nil?(scheme) do
      port = default_port(scheme)
    end

    URI.Info[
      scheme: scheme, path: path, query: query,
      fragment: fragment, authority: authority,
      userinfo: userinfo, host: host, port: port
    ]
  end

  @doc """
  Parses a URI into the following components:

      { <scheme>, <authority>, <path>, <query>, <fragment> }

  if `path`, `query`, or `fragment` are missing in the given string, `nil` is
  returned for the corresponding tuple places.
  """
  def parse_components(uri_string) when is_binary(uri_string) do
    # From http://tools.ietf.org/html/rfc3986#appendix-B
    regex = %r/^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?/
    parts = nillify(Regex.run(regex, uri_string))

    destructure [
      __full_string,
      __scheme_with_colon,
      scheme,
      __authority_with_slashes,
      authority,
      path,
      __query_with_question_mark,
      query,
      __fragment_with_hash,
      fragment
    ], parts

    { scheme, authority, path, query, fragment }
  end

  defp normalize_components({ scheme, authority, path, query, fragment }) do
    scheme = normalize_scheme(scheme)
    authority = authority |> percent_decode() |> normalize_authority()
    path = percent_decode(path)
    query = query
    fragment = percent_decode(fragment)

    { scheme, authority, path, query, fragment }
  end

  defp normalize_authority("") do
    ""
  end

  defp normalize_authority(authority) do
    { userinfo, host, port } = split_authority(authority)

    authority = ""
    if userinfo, do: authority = authority <> userinfo <> "@"
    if host, do: authority = authority <> host
    if port, do: authority = authority <> ":" <> integer_to_binary(port)
    authority
  end

  # Split an authority into its userinfo, host and port parts.
  #
  # Note: some schemes only allow the host part, in which case userinfo and
  # port will be set to `nil`.
  defp split_authority(scheme, nil) do
    { nil, nil, nil }
  end

  defp split_authority(scheme, authority) do
    components = nillify(Regex.run(%r/(^(.*)@)?([^:]*)(:(\d*))?/, authority))
    destructure [_, _, userinfo, host, _, port], components
    port = if port, do: binary_to_integer(port)
    { userinfo, host, port }
  end

  # Regex.run returns empty strings sometimes. We want
  # to replace those with nil for consistency.
  defp nillify(l) do
    lc s inlist l do
      if size(s) > 0, do: s, else: nil
    end
  end

  @doc """
  Verifies that the given string contains a well formed URI.
  """
  def valid?(uri_string) when is_binary(uri_string) do
  end
end

defimpl Binary.Chars, for: URI.Info do
  def to_binary(URI.Info[] = uri) do
    scheme = uri.scheme

    if scheme && (port = URI.default_port(scheme)) do
      if uri.port == port, do: uri = uri.port(nil)
    end

    result = ""

    authority = build_authority(uri)

    if uri.scheme,   do: result = result <> uri.scheme <> "://"
    if authority,    do: result = result <> percent_encode(authority)
    #if uri.userinfo, do: result = result <> uri.userinfo <> "@"
    #if uri.host,     do: result = result <> uri.host
    #if uri.port,     do: result = result <> ":" <> integer_to_binary(uri.port)
    if uri.path,     do: result = result <> percent_encode(uri.path)
    if uri.query,    do: result = result <> "?" <> escape_query(uri.query)
    if uri.fragment, do: result = result <> "#" <> percent_encode(uri.fragment)

    result
  end
end
