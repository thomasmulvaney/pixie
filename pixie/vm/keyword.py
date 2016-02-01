from pixie.vm.object import Object, Type
from pixie.vm.named_object import NamedObject
from pixie.vm.string import String
import pixie.vm.stdlib as proto
from pixie.vm.code import extend, as_var

class Keyword(NamedObject):
    _type = Type(u"pixie.stdlib.Keyword")

    def type(self):
        return Keyword._type

class KeywordCache(object):
    def __init__(self):
        self._cache = {}

    def intern(self, nm):
        kw = self._cache.get(nm, None)

        if kw is None:
            kw = Keyword(nm)
            self._cache[nm] = kw

        return kw

_kw_cache = KeywordCache()

def keyword(nm, ns=None):
    if ns:
        nm = u"/".join([ns, nm])
    return _kw_cache.intern(nm)

@extend(proto._name, Keyword)
def _name(self):
    assert isinstance(self, Keyword)
    return self.get_name()

@extend(proto._namespace, Keyword)
def _namespace(self):
    assert isinstance(self, Keyword)
    return self.get_namespace()

@extend(proto._hash, Keyword)
def _hash(self):
    assert isinstance(self, Keyword)
    return self.get_hash()

@as_var("keyword")
def _keyword(s):
    if not isinstance(s, String):
        from pixie.vm.object import runtime_error
        runtime_error(u"Keyword name must be a string")
    return keyword(s._str)
