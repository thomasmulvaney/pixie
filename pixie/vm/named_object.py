import pixie.vm.object as object
from pixie.vm.primitives import nil
import pixie.vm.rt as rt
import pixie.vm.util as util
from rpython.rlib.rarithmetic import intmask

class NamedObject(object.Object):
    def __init__(self, s, meta=nil):
        self._str = s
        self._w_name = None
        self._w_ns = None
        self._hash = None
        self._meta = meta

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

    def get_string(self):
        return rt.wrap(self._str)

    def get_name(self):
        self.init_names()
        return self._w_name

    def get_namespace(self):
        self.init_names()
        return self._w_ns

    def get_hash(self):
        if self._hash is None:
            self._hash = rt.wrap(intmask(util.hash_unencoded_chars(self._str)))
        return self._hash

