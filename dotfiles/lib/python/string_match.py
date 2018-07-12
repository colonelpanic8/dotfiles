def exact_match(text_body, query):
    for index in range(len(text_body) - len(query) + 1):
        for query_index, query_character in enumerate(query):
            if query_character != text_body[index+query_index]:
                break
        else:
            return True
    return False

def wildcard_match(text_body, query):
    for index in range(len(text_body) - len(query) + 1):
        for query_index, query_character in enumerate(query):
            if (query_character != '.' and
                query_character != text_body[index+query_index]):
                break
        else:
            return True
    return False

def match_maybe(text_body, query):
    if query == "":
        return True
    if len(query) > 1 and query[1] == '?':
        return ((len(text_body) > 0 and text_body[0] == query[0] and match_maybe(text_body[1:], query[2:]))
                or match_maybe(text_body, query[2:]))
    return text_body[0] == query[0] and match_maybe(text_body[1:], query[1:])

def match_maybe_all(text_body, query):
    for starting_index in range(len(text_body)):
        if match_maybe(text_body[starting_index:], query):
            return True
    return False
