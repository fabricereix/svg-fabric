#!/usr/bin/env python3
import yaml
import sys
from yaml import Loader
import json


def generate_elements(elements, attributes):
    elems = []
    for (name, definition) in sorted(elements.items()):
        elem = {'name': name, 'attributes': []}
        for attr in definition['attributes']:
            if isinstance(attr, str):
                elem['attributes'].append(get_attribute(attributes, attr))
            else:
                elem['attributes'].append(attr)
        for attr in find_attributes(attributes, name):
            elem['attributes'].append(get_attribute(attributes, attr))
        elems.append(elem)
    return elems


def eval_attributes(attributes, groups):
    new_attributes = {}
    for name, attribute in attributes.items():
        new_attribute = {'type': attribute['type']}
        new_attribute['default'] = attribute['default'] if 'default' in attribute else None
        if 'elements' in attribute:
            new_elements = []
            for element in attribute['elements']:
                if element.startswith('@'):
                    group = element[1:]
                    if group in groups:
                        for element2 in groups[group]:
                            new_elements.append(element2)
                    else:
                        raise Exception('Group %s not found' % group)
                else:
                    new_elements.append(element)
            new_attribute['elements'] = sorted(new_elements)
        else:
            new_attribute['elements'] = []
        new_attributes[name] = new_attribute
    return new_attributes



def get_attribute(attributes, name):
  if name in attributes:
      return {'name': name, 'type': attributes[name]['type'], 'default': attributes[name]['default']}
  return None


def find_attributes(attributes, element_name):
    attrs = []
    for name, definition in sorted(attributes.items()):
        if 'elements' in definition:
            if element_name in definition['elements']:
               attrs.append(name)
    return attrs


def generate_attributes(attributes):
    attrs = {}
    for attribute_name, definition in attributes.items():
        attribute = {'type': definition['type']}
        if 'default' in definition:
            attribute['default'] = definition['default']
        attrs[attribute_name] = attribute

    attrs2 = []
    for attribute_name, definition in sorted(attrs.items()):
        attr2 = {'name': attribute_name, 'elements': []}
        if 'elements' in definition:
            for elem in definition['elements']:
                if elem.startswith('@'):
                    for elem2 in element_groups[elem[1:]]:
                        elem3 = {'name': elem2}
                        attr2['elements'].append(elem3)
                else:
                    elem3 = {'name': elem}
                    attr2['elements'].append(elem3)
        attrs2.append(attr2)
    return attrs2


def specific_attributes(elements):
    attributes = {}
    for element_name, element in elements.items():
        for attribute in element['attributes']:
            if isinstance(attribute, dict):
                attributes[attribute['name']] = {element_name: {'type': attribute['type'], 'default': attribute['default'] }}
    return attributes


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
