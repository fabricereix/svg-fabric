#!/usr/bin/env python3
import sys
import json
from jinja2 import Template, Environment, FileSystemLoader
# import template_utils
import os
import glob


def wordDigit(i):
    if i == 1:
        return "One"
    elif i == 2:
        return "Two"
    elif i == 3:
        return "Three"
    else:
        raise Exception('unsupported')


def capitalize(s):
    if s == '':
        return ''
    return s[0].upper() + s[1:]


def type_arguments(t):
    arguments = {
      'length': ['Length Double'],
      'percentage': ['Percentage Double'],
      'number': ['Number Double'],
      'paint': ['Color String'],
      'points': ['Points [(Double,Double)]'],
      'viewbox': ['Viewbox Double Double Double Double'],
      'auto': ['AUTO'],
      'removeFreeze': ['REMOVE', 'FREEZE'],
      'path': ['Path [Command]']
    }
    if t not in arguments:
        raise Exception('type %s not defined' % t)
    return arguments[t]


def flatMap(x):
    flatten = []
    for x in a:
        for y in x:
            flatten.append(y)
    return flatten


def concatMap(f, xs):
    flatten = []
    for x in xs:
        for y in f(x):
            flatten.append(y)
    return flatten


def main():

    if len(sys.argv) < 3:
        print('usage: python generate_src_elements SPEC_JSON TEMPLATES_DIR GENERATED_DIR')
        sys.exit(1)

    spec_file = sys.argv[1]
    templates_dir = sys.argv[2]
    generated_dir = sys.argv[3]

    spec = json.loads(open(spec_file).read())
    environment = Environment(loader=FileSystemLoader(templates_dir))
    # environment.filters['camel_case'] = template_utils.camel_case
    # environment.filters['size'] = template_utils.size
    environment.filters['map_name'] = lambda items: [item['name'] for item in items]
    environment.filters['map_length'] = lambda items: [len(item) for item in items]
    environment.filters['max_attribute_name'] = lambda elem: max([
        len(attr['name']) for attr in elem['attributes']
    ])
    environment.filters['toWord'] = lambda i: "One" if i == 1 else (
        "Two" if i == 2 else ("Three" if i == 3 else "??"))
    environment.filters['default_attr'] = lambda x: "AUTO" if x == "auto" else (
        "(Length 0)" if x == 0 else ("REMOVE" if x == "remove" else "??"))

    for template_file in glob.glob(templates_dir + '/**/*.hs', recursive=True):
        template_id = template_file[len(templates_dir)+1:]
        output_file = generated_dir + '/' + template_id
        if '{{element}}' in template_id:
            for element in spec['elements']:
                output_file = generated_dir + '/' + template_id.replace(
                  '{{element}}',
                  element['name'].capitalize())
                print('eval %s to %s' % (template_file, output_file))
                template = environment.get_template(template_id)
                os.makedirs(os.path.dirname(output_file), exist_ok=True)
                open(output_file, 'w').write(template.render(
                  element=element,
                  enumerate=enumerate,
                  wordDigit=wordDigit,
                  capitalize=capitalize,
                  type_arguments=type_arguments,
                  len=len,
                  flatMap=flatMap,
                  concatMap=concatMap
                ))
        else:
            output_file = generated_dir + '/' + template_id
            print('eval %s to %s' % (template_file, output_file))
            template = environment.get_template(template_id)
            os.makedirs(os.path.dirname(output_file), exist_ok=True)
            open(output_file, 'w').write(template.render(
                elements=spec['elements'],
                name=lambda x: x['name'],
                len=len,
                max=max,
                map=map,
                enumerate=enumerate,
                capitalize=capitalize))


if __name__ == '__main__':
    main()
