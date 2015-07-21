from pixie.vm.object import Object, Type
from pixie.vm.primitives import nil
from pixie.vm.string import String
import pixie.vm.stdlib as proto
from pixie.vm.code import extend, as_var
import pixie.vm.rt as rt
import pixie.vm.util as util
from rpython.rlib.rarithmetic import intmask


class Keyword(Object):
    _type = Type(u"pixie.stdlib.Keyword")
    def __init__(self, name):
        self._str = name
        self._w_name = None
        self._w_ns = None
        self._hash = 0

    def type(self):
        return Keyword._type

    def init_names(self):
        if self._w_name is None:
            s = self._str.split(u"/")
            if len(s) == 2:
                self._w_ns = rt.wrap(s[0])
                self._w_name = rt.wrap(s[1])
            elif len(s) == 1:
                self._w_name = rt.wrap(s[0])
                self._w_ns = nil
            else:
                self._w_ns = rt.wrap(s[0])
                self._w_name = rt.wrap(u"/".join(s[1:]))


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
    self.init_names()
    return self._w_name

@extend(proto._namespace, Keyword)
def _namespace(self):
    assert isinstance(self, Keyword)
    self.init_names()
    return self._w_ns

@extend(proto._hash, Keyword)
def _hash(self):
    assert isinstance(self, Keyword)
    if self._hash == 0:
        self._hash = util.hash_unencoded_chars(self._str)
    return rt.wrap(intmask(self._hash))

@as_var("keyword")
def _keyword(s):
    if not isinstance(s, String):
        from pixie.vm.object import runtime_error
        runtime_error(u"Keyword name must be a string")
    return keyword(s._str)
