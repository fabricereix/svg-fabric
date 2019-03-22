#!/usr/bin/env python3
import yaml
import sys
from yaml import Loader
import json


def generate_elements(elements, element_groups, attributes):
    elems = []
    for (name, definition) in sorted(elements.items()):
        elem = {'name': name, 'attributes': []}
        for attr in definition['attributes']:
            if isinstance(attr, str):
               elem['attributes'].append(get_attribute(attributes, attr))
        for attr in find_attributes(attributes, element_groups, name):
            elem['attributes'].append(get_attribute(attributes, attr))
        elems.append(elem)
    return elems


def get_attribute(attributes, name):
  if name in attributes:
      attr = {'name': name, 'type': attributes[name]['type']}
      if 'default' in attributes[name]:
          attr['default'] = attributes[name]['default']
      return attr
  return None


def find_attributes(attributes, element_groups, element_name):
    attrs = []
    for name, definition in sorted(attributes.items()):
        if 'elements' in definition:
            if element_name in definition['elements']:
               attrs.append(name)
            for element in definition['elements']:
                if element.startswith('@'):
                    if element[1:] in element_groups and element_name in element_groups[element[1:]]:
                        attrs.append(name)
    return attrs


def generate_attributes(attributes):
    return []


def main():
    s = sys.stdin.read()
    spec = yaml.load(s, Loader=Loader)
    #print(spec)

    groups = spec['element-groups']
    groups['all'] = [elem for elem in sorted(spec['elements'])]
    elements = generate_elements(spec['elements'], groups, spec['attributes'])
    attributes = generate_attributes(spec['attributes'])

    print(json.dumps({'elements': elements, 'attributes': attributes}))


if __name__ == '__main__':
    main()
