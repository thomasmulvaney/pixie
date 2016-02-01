from pixie.vm.named_object import NamedObject
import pixie.vm.object as object
from pixie.vm.primitives import true, false
import pixie.vm.stdlib as proto
from pixie.vm.code import extend, as_var
from pixie.vm.string import String
import pixie.vm.rt as rt

class Symbol(NamedObject):
    _type = object.Type(u"pixie.stdlib.Symbol")

    def type(self):
        return Symbol._type

    def with_meta(self, meta):
        return Symbol(self._str, meta)

    def meta(self):
        return self._meta

@extend(proto._eq, Symbol)
def _eq(self, other):
    assert isinstance(self, Symbol)
    if not isinstance(other, Symbol):
        return false
    return true if self._str == other._str else false

@extend(proto._str, Symbol)
def _str(self):
    assert isinstance(self, Symbol)
    return self.get_string()

@extend(proto._name, Symbol)
def _name(self):
    assert isinstance(self, Symbol)
    return self.get_name()

@extend(proto._namespace, Symbol)
def _namespace(self):
    assert isinstance(self, Symbol)
    return self.get_namespace()

@extend(proto._hash, Symbol)
def _hash(self):
    assert isinstance(self, Symbol)
    return self.get_hash()


@as_var("symbol")
def _symbol(s):
    if not isinstance(s, String):
        from pixie.vm.object import runtime_error
        runtime_error(u"Symbol name must be a string")
    return Symbol(s._str)

@extend(proto._meta, Symbol)
def _meta(self):
    assert isinstance(self, Symbol)
    return self.meta()

@extend(proto._with_meta, Symbol)
def _with_meta(self, meta):
    assert isinstance(self, Symbol)
    return self.with_meta(meta)
