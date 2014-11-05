from cached_property import cached_property


class XPathBuilder(object):

    def __init__(self, nodes=(), relative=True, direct_child=False):
        self.nodes = tuple(nodes)
        self.relative = relative
        self.direct_child = direct_child

    @cached_property
    def xpath(self):
        return ('.' if self.relative else '') + ''.join(node.xpath
                                                        for node in self.nodes)

    @property
    def or_(self):
        return self.update_final_node(self.nodes[-1].make_or)

    @property
    def text_(self):
        return self.update_final_node(
            self.nodes[-1](selected_attribute=XPathNode.text)
        )

    def add_node(self, **kwargs):
        if 'direct_child' not in kwargs:
            kwargs['direct_child'] = self.direct_child
        return type(self)(self.nodes + (XPathNode(**kwargs),),
                          relative=self.relative)

    def __getattr__(self, attr):
        return self.add_node(element=attr)

    def update_final_node(self, updated_final_node):
        return type(self)(self.nodes[:-1] + (updated_final_node,),
                          relative=self.relative,
                          direct_child=self.direct_child)

    def __call__(self, *predicates, **attributes):
        direct_child = attributes.pop('direct_child', None)
        assert len(self.nodes)
        updated_final_node = self.nodes[-1](predicates=predicates,
                                            attributes=attributes,
                                            direct_child=direct_child)
        return self.update_final_node(updated_final_node)

    def attribute_contains(self, attribute, contains_string):
        updated_final_node = self.nodes[-1].add_contains_predicates(
            ((attribute, contains_string),)
        )
        return self.update_final_node(updated_final_node)

    def with_classes(self, *classes):
        return self.update_final_node(self.nodes[-1].with_classes(classes))

    def select_attribute_(self, attribute, elem=None):
        update_final_node = self.nodes[-1](selected_attribute=attribute)
        builder = self.update_final_node(update_final_node)
        if elem is not None:
            return builder.apply_(elem)
        else:
            return builder

    def text_contains_(self, contained_text):
        updated_final_node = self.nodes[-1].text_contains(contained_text)
        return self.update_final_node(updated_final_node)

    with_class = with_classes

    def apply_(self, tree):
        return tree.xpath(self.xpath)

    def one_(self, tree):
        return self.apply_(tree)[0]

    def get_text_(self, tree):
        return self.apply_(tree)[0].text_content()

    def __repr__(self):
        return '{0}("{1}")'.format(type(self).__name__, self.xpath)


class XPathNode(object):

    text = object()

    @staticmethod
    def contains_class(class_attribute, contained_class):
        return "contains(concat(' ',normalize-space(@{0}),' '),' {1} ')".\
            format(class_attribute, contained_class)

    @staticmethod
    def contains_attribute(attribute, contained_string):
        return "contains(@{0}, '{1}')".format(attribute, contained_string)

    @staticmethod
    def attribute_equal(attribute, value):
        return "@{0} = '{1}'".format(attribute, value)

    def __init__(self, element='*', attributes=None, predicates=None,
                 direct_child=False, use_or=False, selected_attribute=None):
        self.element = element
        self.predicates = tuple(predicates) if predicates else ()
        if attributes:
            self.predicates += tuple([self.attribute_equal(key, value)
                                      for key, value in attributes.items()])
        self.direct_child = direct_child
        self.use_or = use_or
        self.selected_attribute = selected_attribute

    @property
    def make_or(self):
        return self(use_or=True)

    @property
    def separator(self):
        return '/' if self.direct_child else '//'

    @property
    def xpath(self):
        return '{0}{1}{2}{3}'.format(self.separator, self.element,
                                     self.predicate_string,
                                     self.selected_attribute_string)

    @property
    def predicate_joiner(self):
        return ' or ' if self.use_or else ' and '

    @property
    def predicate_string(self):
        if self.predicates:
            predicate = self.predicate_joiner.join(self.predicates)
            return '[ {0} ]'.format(predicate)
        else:
            return ''

    @property
    def selected_attribute_string(self):
        if self.selected_attribute is self.text:
            return '/text()'
        return '/@{0}'.format(self.selected_attribute) \
            if self.selected_attribute else ''

    def __call__(self, element=None, predicates=(), attributes=None,
                 direct_child=None, use_or=False, selected_attribute=None):
        direct_child = (self.direct_child
                        if direct_child is None
                        else direct_child)
        element = self.element if element is None else element
        new_predicates = self.predicates + tuple(predicates)
        return type(self)(element, attributes, new_predicates,
                          direct_child, use_or, selected_attribute)

    def with_classes(self, classes):
        predicates = tuple(self.contains_class('class', contained_class)
                           for contained_class in classes)

        return self(predicates=predicates)

    def add_contains_predicates(self, kv_pairs):
        predicates = tuple(self.contains_attribute(attribute, contains_string)
                           for attribute, contains_string in kv_pairs)
        return self(predicates=predicates)

    def text_contains(self, contained_text):
        return self(predicates=("contains(text(),'{0}')".
                                format(contained_text),))


xpb = XPathBuilder()
dxpb = XPathBuilder(direct_child=True)
