#!/usr/bin/env python3
import sys
import json
from jinja2 import Template, Environment, FileSystemLoader
import template_utils
import os


BUILD_DIR = 'build'

def main():

    if len(sys.argv) < 2:
        print('usage: python generate_src_elements TEMPLATE_FILE SPEC_SVG')
        sys.exit(1)

    spec = json.loads(open(sys.argv[1]).read())

    environment = Environment(loader=FileSystemLoader('templates/'))
    environment.filters['camel_case'] = template_utils.camel_case

    for template_file in sys.argv[2:]:

        template = environment.get_template(template_file)
        output_file = BUILD_DIR + '/' + template_file
        print('eval template to ' + output_file)
        os.makedirs(os.path.dirname(output_file), exist_ok=True)
        open(output_file, 'w').write(template.render(elements=spec['elements']))


if __name__ == '__main__':
    main()
