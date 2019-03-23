#!/usr/bin/env python3
import unittest
import spec2json

ELEMENTS = {
  'svg': {
    'attributes': ['width']
  },
  'rect': {
    'attributes': ['x', 'width']
  },
  'circle': {
    'attributes': ['r']
  }
}

ELEMENT_GROUPS = {
    'graphics': ['rect', 'circle']
}
ELEMENT_GROUPS['all'] = [elem for elem in ELEMENTS]

ATTRIBUTES = {
  'fill': { 'type': ['paint'], 'default': 'black', 'elements': ['rect']},
  'id': {'type': ['string'], 'elements': ['@all']},
  'opacity': {'type': ['opacity'], 'elements': ['@graphics']},
  'width': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
  'r': {'type': ['length', 'percentage'], 'default': 0},
  'x': {'type': ['length', 'percentage'], 'default': 0}
}


ATTRIBUTES2 = {
  'fill': { 'type': ['paint'], 'default': 'black', 'elements': ['rect']},
  'id': {'type': ['string'], 'elements': ['circle', 'rect', 'svg']},
  'opacity': {'type': ['opacity'], 'elements': ['circle', 'rect']},
  'width': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
  'r': {'type': ['length', 'percentage'], 'default': 0},
  'x': {'type': ['length', 'percentage'], 'default': 0}
}

ATTRIBUTES1 = {
  'fill': { 'type': ['paint'], 'default': 'black', 'elements': ['rect']},
  'id': {'type': ['string'], 'elements': ['@all']},
  'opacity': {'type': ['opacity'], 'elements': ['@graphics']},
  'width': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
  'r': {'type': ['length', 'percentage'], 'default': 0},
  'x': {'type': ['length', 'percentage'], 'default': 0}
}


class SpecToJsonTest(unittest.TestCase):


    def test_eval_element_groups(self):
        self.assertEqual(ATTRIBUTES2, spec2json.eval_element_groups(ATTRIBUTES1, ELEMENT_GROUPS))

    def test_get_attribute(self):
        self.assertEqual({'name': 'x', 'type': ['length', 'percentage'], 'default': 0},
                spec2json.get_attribute(ATTRIBUTES, 'x'))

    def test_generate_elements(self):
        #print(generate_elements(ELEMENTS, ATTRIBUTES))
        self.assertEqual([
            {
              'name': 'circle',
              'attributes': [
                  {'name': 'r', 'type':['length', 'percentage'], 'default':0},
                  {'name': 'id', 'type':['string']},
                  {'name': 'opacity', 'type':['opacity']}
              ]
            },
            {
              'name': 'rect',
              'attributes': [
                  {'name': 'x', 'type':['length', 'percentage'], 'default':0},
                  {'name': 'width', 'type':['auto', 'length', 'percentage'], 'default':'auto'},
                  {'name': 'fill', 'type':['paint'], 'default':'black'},
                  {'name': 'id', 'type':['string']},
                  {'name': 'opacity', 'type':['opacity']}
              ]
            },
            {
              'name': 'svg',
              'attributes': [
                  {'name': 'width', 'type':['auto', 'length', 'percentage'], 'default':'auto'},
                  {'name': 'id', 'type':['string']}
              ]
            },
        ], spec2json.generate_elements(ELEMENTS, ATTRIBUTES2, ELEMENT_GROUPS))


    def test_find_attribute(self):
        self.assertEqual(['fill', 'id','opacity'], spec2json.find_attributes(ATTRIBUTES2,'rect'))
        self.assertEqual([], spec2json.find_attributes(ATTRIBUTES2,'invalid'))

    def test_generate_attributes(self):
        self.assertEqual([
            { 'name': 'fill', 'elements': [

                ]},
            { 'name': 'id', 'elements': [

                ]},
            { 'name': 'opacity', 'elements': [

                ]},
            { 'name': 'r', 'elements': [

                ]},
            { 'name': 'width', 'elements': [

                ]},
            { 'name': 'x', 'elements': [

                ]}
            ], spec2json.generate_attributes(ATTRIBUTES))

if __name__ == '__main__':
    unittest.main()
