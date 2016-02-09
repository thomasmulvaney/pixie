py_object = object
import pixie.vm.object as object
from pixie.vm.primitives import nil, true, false
import pixie.vm.persistent_hash_map as persistent_hash_map
import pixie.vm.stdlib as proto
from  pixie.vm.code import extend, as_var, intern_var
import pixie.vm.rt as rt


VAR_KEY = intern_var(u"pixie.stdlib", u"key")

class PersistentHashSet(object.Object):
    _type = object.Type(u"pixie.stdlib.PersistentHashSet")

    def __init__(self, meta, m):
        self._meta = meta
        self._map = m

    def conj(self, v):
        return PersistentHashSet(self._meta, self._map.assoc(v, v))

    def disj(self, k):
        return PersistentHashSet(self._meta, self._map.without(k))

    def meta(self):
        return self._meta

    def with_meta(self, meta):
        return PersistentHashSet(meta, self._map)

EMPTY = PersistentHashSet(nil, persistent_hash_map.EMPTY)

@as_var("set")
def _create(coll):
    ret = EMPTY
    coll = rt._seq(coll)
    while coll is not nil:
        ret = ret.conj(rt._first(coll))
        coll = rt._seq(rt._next(coll))
    return ret

@extend(proto._count, PersistentHashSet)
def _count(self):
    assert isinstance(self, PersistentHashSet)
    return rt._count(self._map)

@extend(proto._val_at, PersistentHashSet)
def _val_at(self, key, not_found):
    assert isinstance(self, PersistentHashSet)
    return rt._val_at(self._map, key, not_found)

@extend(proto._contains_key, PersistentHashSet)
def _contains_key(self, key):
    assert isinstance(self, PersistentHashSet)
    return rt._contains_key(self._map, key)

@extend(proto._eq, PersistentHashSet)
def _eq(self, obj):
    assert isinstance(self, PersistentHashSet)
    if self is obj:
        return true
    if not isinstance(obj, PersistentHashSet):
        return false
    if self._map._cnt != obj._map._cnt:
        return false

    seq = rt.seq(obj)
    while seq is not nil:
        if rt._contains_key(self, rt.first(seq)) is false:
            return false
        seq = rt.next(seq)
    return true

@extend(proto._conj, PersistentHashSet)
def _conj(self, v):
    assert isinstance(self, PersistentHashSet)
    return self.conj(v)

@extend(proto._disj, PersistentHashSet)
def _disj(self, v):
    assert isinstance(self, PersistentHashSet)
    return self.disj(v)

@extend(proto._reduce, PersistentHashSet)
def _reduce(self, f, init):
    assert isinstance(self, PersistentHashSet)
    return rt._reduce(rt.keys(self._map), f, init)

@extend(proto._meta, PersistentHashSet)
def _meta(self):
    assert isinstance(self, PersistentHashSet)
    return self.meta()

@extend(proto._with_meta, PersistentHashSet)
def _with_meta(self, meta):
    assert isinstance(self, PersistentHashSet)
    return self.with_meta(meta)
