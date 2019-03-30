#!/usr/bin/env python3
import unittest
import spec2json

ELEMENTS_INPUT = {
    'animate': {
        'attributes': [
            {'name': 'fill', 'type': 'remove_freeze', 'default': 'remove'}
        ]
    },
    'circle': {
        'attributes': ['r']
    },
    'rect': {
        'attributes': ['x', 'width']
    },
    'svg': {
        'attributes': ['width']
    }
}

ELEMENT_GROUPS = {
    'graphics': ['rect', 'circle'],
    'all': [elem for elem in ELEMENTS_INPUT]
}

ATTRIBUTES = {
    'fill': {'type': ['paint'], 'default': 'black', 'elements': ['rect']},
    'id': {'type': ['string'], 'default': None, 'elements': ['animate', 'circle', 'rect', 'svg']},
    'opacity': {'type': ['opacity'], 'default': None, 'elements': ['circle', 'rect']},
    'width': {'type': ['auto', 'length', 'percentage'], 'default': 'auto', 'elements': []},
    'r': {'type': ['length', 'percentage'], 'default': 0, 'elements': []},
    'x': {'type': ['length', 'percentage'], 'default': 0, 'elements': []}
}

ATTRIBUTES_INPUT = {
    'fill': {'type': ['paint'], 'default': 'black', 'elements': ['rect']},
    'id': {'type': ['string'], 'elements': ['@all']},
    'opacity': {'type': ['opacity'], 'elements': ['@graphics']},
    'width': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
    'r': {'type': ['length', 'percentage'], 'default': 0},
    'x': {'type': ['length', 'percentage'], 'default': 0}
}

SPECIFIC_ATTRIBUTES = {
    'fill': {
        'animate': {'type': ['remove_freeze'], 'default': 'remove'}
    },
    'r': {
        'circle': {'type': ['length', 'percentage'], 'default': 0}
    },
    'x': {
        'rect': {'type': ['length', 'percentage'], 'default': 0}
    },
    'width': {
        'rect': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
        'svg': {'type': ['auto', 'length', 'percentage'], 'default': 'auto'}
    },
}


class SpecToJsonTest(unittest.TestCase):

    def test_eval_attributes(self):
        self.assertEqual(ATTRIBUTES, spec2json.eval_attributes(ATTRIBUTES_INPUT, ELEMENT_GROUPS))

    def test_get_attribute(self):
        self.assertEqual({'name': 'x', 'type': ['length', 'percentage'], 'default': 0},
                         spec2json.get_attribute(ATTRIBUTES, 'x'))

    def test_generate_elements(self):
        # print(spec2json.generate_elements(ELEMENTS, ATTRIBUTES))
        self.assertEqual([
            {
                'name': 'animate',
                'attributes': [
                    {'name': 'fill', 'type': ['remove_freeze'], 'default': 'remove'},
                    {'name': 'id', 'type': ['string'], 'default': None}
                ]
            },
            {
                'name': 'circle',
                'attributes': [
                    {'name': 'r', 'type': ['length', 'percentage'], 'default': 0},
                    {'name': 'id', 'type': ['string'], 'default': None},
                    {'name': 'opacity', 'type': ['opacity'], 'default': None}
                ]
            },
            {
                'name': 'rect',
                'attributes': [
                    {'name': 'x', 'type': ['length', 'percentage'], 'default': 0},
                    {'name': 'width', 'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
                    {'name': 'fill', 'type': ['paint'], 'default': 'black'},
                    {'name': 'id', 'type': ['string'], 'default': None},
                    {'name': 'opacity', 'type': ['opacity'], 'default': None}
                ]
            },
            {
                'name': 'svg',
                'attributes': [
                    {'name': 'width', 'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
                    {'name': 'id', 'type': ['string'], 'default': None}
                ]
            },
        ], spec2json.generate_elements(ELEMENTS_INPUT, ATTRIBUTES))

    def test_find_attribute(self):
        self.assertEqual(['fill', 'id', 'opacity'], spec2json.find_attributes(ATTRIBUTES, 'rect'))
        self.assertEqual([], spec2json.find_attributes(ATTRIBUTES, 'invalid'))

    def test_specific_attributes(self):
        self.assertEqual(SPECIFIC_ATTRIBUTES, spec2json.specific_attributes(ELEMENTS_INPUT, ATTRIBUTES))

    def test_generate_attributes(self):
        self.assertEqual([
            {
                'name': 'fill',
                'elements': [
                    {'element': 'animate', 'type': ['remove_freeze'], 'default': 'remove'},
                    {'element': 'rect', 'type': ['paint'], 'default': 'black'}
                ]
            },
            {
                'name': 'id',
                'elements': [
                    {'element': 'animate', 'type': ['string'], 'default': None},
                    {'element': 'circle', 'type': ['string'], 'default': None},
                    {'element': 'rect', 'type': ['string'], 'default': None},
                    {'element': 'svg', 'type': ['string'], 'default': None}
                ]
            },
            {
                'name': 'opacity',
                'elements': [
                    {'element': 'circle', 'type': ['opacity'], 'default': None},
                    {'element': 'rect', 'type': ['opacity'], 'default': None}
                ]
            },
            {
                'name': 'r',
                'elements': [
                    {'element': 'circle', 'type': ['length', 'percentage'], 'default': 0}
                ]
            },
            {
                'name': 'width',
                'elements': [
                    {'element': 'rect', 'type': ['auto', 'length', 'percentage'], 'default': 'auto'},
                    {'element': 'svg', 'type': ['auto', 'length', 'percentage'], 'default': 'auto'}
                ]
            },
            {
                'name': 'x',
                'elements': [
                    {'element': 'rect', 'type': ['length', 'percentage'], 'default': 0}
                ]
            }
        ], spec2json.generate_attributes(ATTRIBUTES, SPECIFIC_ATTRIBUTES))


if __name__ == '__main__':
    unittest.main()

