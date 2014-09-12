class XPathBuilder(object):

    def __init__(self, nodes=(), relative=True, direct_child=False):
        self.nodes = tuple(nodes)
        self.relative = relative
        self.direct_child = direct_child

    @property
    def xpath(self):
        return ('.' if self.relative else '') + ''.join(node.xpath
                                                        for node in self.nodes)

    @property
    def _or(self):
        updated_final_node = self.nodes[-1].make_or
        return type(self)(self.nodes[:-1] + (updated_final_node,),
                          relative=self.relative, direct_child=self.direct_child)

    def add_node(self, **kwargs):
        if 'direct_child' not in kwargs:
            kwargs['direct_child'] = self.direct_child
        return type(self)(self.nodes + (XPathNode(**kwargs),),
                          relative=self.relative)

    def __getattr__(self, attr):
        return self.add_node(element=attr)

    def __call__(self, *predicates, **attributes):
        direct_child = attributes.pop('direct_child', None)
        assert len(self.nodes)
        updated_final_node = self.nodes[-1](predicates=predicates,
                                            attributes=attributes,
                                            direct_child=direct_child)
        return type(self)(self.nodes[:-1] + (updated_final_node,),
                          relative=self.relative, direct_child=self.direct_child)

    def attribute_contains(self, attribute, contains_string):
        updated_final_node = self.nodes[-1].add_contains_predicates(((attribute, contains_string),))
        return type(self)(self.nodes[:-1] + (updated_final_node,),
                          relative=self.relative, direct_child=self.direct_child)

    def with_classes(self, *classes):
        updated_final_node = self.nodes[-1].with_classes(classes)
        return type(self)(self.nodes[:-1] + (updated_final_node,),
                          relative=self.relative, direct_child=self.direct_child)

    with_class = with_classes

    def apply_(self, tree):
        return tree.xpath(self.xpath)

    def get_text_(self, tree):
        return self.apply_(tree)[0].text


class XPathNode(object):

    @staticmethod
    def attribute_contains(attribute, contained_string):
        return "contains(concat(' ',normalize-space(@{0}),' '),' {1} ')".format(
            attribute, contained_string
        )

    @staticmethod
    def attribute_equal(attribute, value):
        return "@{0} = '{1}'".format(attribute, value)

    def __init__(self, element='*', attributes=None, predicates=None,
                 direct_child=False, use_or=False):
        self.element = element
        self.predicates = tuple(predicates) if predicates else ()
        if attributes:
            self.predicates += tuple([self.attribute_equal(attribute, value)
                                      for attribute, value in attributes.items()])
        self.direct_child = direct_child
        self.use_or = use_or

    @property
    def make_or(self):
        return self(use_or=True)

    @property
    def separator(self):
        return '/' if self.direct_child else '//'

    @property
    def xpath(self):
        return '{0}{1}{2}'.format(self.separator, self.element,
                                  self.predicate_string)

    @property
    def predicate_joiner(self):
        return ' or ' if self.use_or else ' and '

    @property
    def predicate_string(self):
        if self.predicates:
            predicate =  self.predicate_joiner.join(self.predicates)
            return '[ {0} ]'.format(predicate)
        else:
            return ''

    def __call__(self, element=None, predicates=(), attributes=None,
                 direct_child=None, use_or=False):
        direct_child = self.direct_child if direct_child is None else direct_child
        element = self.element if element is None else element
        new_predicates = self.predicates + tuple(predicates)
        return type(self)(element, attributes, new_predicates,
                          direct_child, use_or)

    def add_contains_predicates(self, kv_pairs):
        predicates = [self.attribute_contains(attribute, contains_string)
                      for attribute, contains_string in kv_pairs]
        return self(predicates=predicates)

    def with_classes(self, classes):
        return self.add_contains_predicates(('class', class_string)
                                     for class_string in classes)
