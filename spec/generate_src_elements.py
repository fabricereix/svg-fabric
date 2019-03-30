#!/usr/bin/env python3
import sys
import json
from jinja2 import Template


def main():

    if len(sys.argv) < 2:
        print('usage: python generate_src_elements TEMPLATE_FILE SPEC_SVG')
        sys.exit(1)

    template_file = sys.argv[1]
    template = Template(open(template_file).read())
    spec = json.loads(open(sys.argv[2]).read())

    print(template.render(elements=spec['elements']))


if __name__ == '__main__':
    main()
