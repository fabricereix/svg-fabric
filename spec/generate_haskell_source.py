#!/usr/bin/env python3
import sys
import json
from jinja2 import Template, Environment, FileSystemLoader
import template_utils
import os
import glob


def main():

    if len(sys.argv) < 3:
        print('usage: python generate_src_elements SPEC_JSON TEMPLATES_DIR GENERATED_DIR')
        sys.exit(1)

    spec_file = sys.argv[1]
    templates_dir = sys.argv[2]
    generated_dir = sys.argv[3]

    spec = json.loads(open(spec_file).read())
    environment = Environment(loader=FileSystemLoader(templates_dir))
    environment.filters['camel_case'] = template_utils.camel_case
    environment.filters['size'] = template_utils.size
    environment.filters['max_attribute_name'] = lambda elem: max([
        len(attr['name']) for attr in elem['attributes']
    ])
    environment.filters['toWord'] = lambda i: "One" if i == 1 else (
        "Two" if i == 2 else ("Three" if i == 3 else "??"))
    environment.filters['default_attr'] = lambda x: "AUTO" if x == "auto" else (
        "Length 0" if x == 0 else ("REMOVE" if x == "remove" else "??"))

    for template_file in glob.glob(templates_dir + '/**/*.hs', recursive=True):
        template_id = template_file[len(templates_dir)+1:]
        output_file = generated_dir + '/' + template_id
        print('eval %s to %s' % (template_file, output_file))
        template = environment.get_template(template_id)
        os.makedirs(os.path.dirname(output_file), exist_ok=True)
        open(output_file, 'w').write(template.render(elements=spec['elements']))


if __name__ == '__main__':
    main()
