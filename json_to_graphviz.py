import json
import sys

def json_to_graphviz(file_obj):
    root = json.load(file_obj)
    template = "digraph G { %s }"

    string = ""
    for key in root:
        for mod in root[key]:
            new_str = "%s -> %s;\n" % (key, mod)
            new_str = new_str.replace('__MAIN__.', '')
            new_str = new_str.replace('.', '_')
            string += new_str.replace('Node', 'Node_')

    return template % string

if __name__ == '__main__':
    with open(sys.argv[1], 'rb') as json_file:
        graphviz = json_to_graphviz(json_file)

    print graphviz
