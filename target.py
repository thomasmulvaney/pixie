from pixie.vm.compiler import with_ns, NS_VAR
from pixie.vm.reader import StringReader
from rpython.jit.codewriter.policy import JitPolicy
from rpython.rlib.jit import JitHookInterface, Counters
from rpython.rlib.rfile import create_stdio
from rpython.annotator.policy import AnnotatorPolicy
from pixie.vm.code import wrap_fn, NativeFn, intern_var 
from pixie.vm.object import WrappedException
from pixie.vm.primitives import nil
from pixie.vm.atom import Atom
from pixie.vm.persistent_vector import EMPTY as EMPTY_VECTOR
from pixie.vm.util import unicode_from_utf8
import sys
import os
import os.path as path
import rpython.rlib.rpath as rpath

class DebugIFace(JitHookInterface):
    def on_abort(self, reason, jitdriver, greenkey, greenkey_repr, logops, operations):
        # print "Aborted Trace, reason: ", Counters.counter_names[reason], logops, greenkey_repr
        pass

import pdb

class Policy(JitPolicy, AnnotatorPolicy):
    def __init__(self):
        JitPolicy.__init__(self, DebugIFace())

def jitpolicy(driver):
    return JitPolicy(jithookiface=DebugIFace())


PROGRAM_NAME = intern_var(u"pixie.stdlib", u"program-name")
PROGRAM_NAME.set_root(nil)

PROGRAM_ARGUMENTS = intern_var(u"pixie.stdlib", u"program-arguments")
PROGRAM_ARGUMENTS.set_root(nil)

LOAD_PATHS = intern_var(u"pixie.stdlib", u"load-paths")
LOAD_PATHS.set_root(nil)
load_path = intern_var(u"pixie.stdlib", u"internal-load-path")

class ReplFn(NativeFn):
    def __init__(self, args):
        self._argv = args

    def inner_invoke(self, args):
        import pixie.vm.rt as rt
        rt.load_ns(rt.wrap(u"pixie/repl.pxi"))

        repl = intern_var(u"pixie.repl", u"repl")
        with with_ns(u"user"):
            repl.invoke([])

load_file = intern_var(u"pixie.stdlib", u"load-file")

class BatchModeFn(NativeFn):
    def __init__(self, args):
        self._file = args[0]
        self._argv = args[1:]

    def inner_invoke(self, args):
        import pixie.vm.rt as rt

        with with_ns(u"user"):
            NS_VAR.deref().include_stdlib()

        acc = EMPTY_VECTOR
        for x in self._argv:
            acc = rt.conj(acc, rt.wrap(x))

        PROGRAM_NAME.set_root(rt.wrap(self._file))
        PROGRAM_ARGUMENTS.set_root(acc)

        with with_ns(u"user"):
            try:
                f = None
                if self._file == '-':
                    f, _, _ = create_stdio()
                else:
                    if not path.isfile(self._file):
                        print "Error: Cannot open '" + self._file + "'"
                        os._exit(1)
                    load_file.invoke([rt.wrap(self._file)])
                    return None

                data = f.read()
                f.close()

                if data.startswith("#!"):
                    newline_pos = data.find("\n")
                    if newline_pos > 0:
                        data = data[newline_pos:]

                rt.load_reader(StringReader(unicode_from_utf8(data)))
            except WrappedException as ex:
                print "Error: ", ex._ex.__repr__()
                os._exit(1)

class EvalFn(NativeFn):
    def __init__(self, expr):
        self._expr = expr

    def inner_invoke(self, args):
        import pixie.vm.rt as rt

        with with_ns(u"user"):
            NS_VAR.deref().include_stdlib()

            rt.load_reader(StringReader(unicode_from_utf8(self._expr)))

class CompileFileFn(NativeFn):
    def __init__(self, filename):
        self._filename = filename

    def inner_invoke(self, args):
        import pixie.vm.rt as rt

        rt.compile_file(rt.wrap(self._filename))


class IsPreloadFlag(object):
    def __init__(self):
        self._is_true = False

    def is_true(self):
        return self._is_true

    def set_true(self):
        self._is_true = True


stdlib_loaded = IsPreloadFlag()

@wrap_fn
def run_load_stdlib():
    global stdlib_loaded
    if stdlib_loaded.is_true():
        return

    rt.load_ns(rt.wrap(u"pixie/stdlib.pxi"))
    rt.load_ns(rt.wrap(u"pixie/stacklets.pxi"))

    stdlib_loaded.set_true()

def load_stdlib():
    run_load_stdlib.invoke([])

run_with_stacklets = intern_var(u"pixie.stacklets", u"run-with-stacklets")

def init_vm(progname):
    import pixie.vm.stacklet
    pixie.vm.stacklet.init()

    init_load_path(progname)
    load_stdlib()
    add_to_load_paths(".")

def entry_point(args):
    try:

        init_vm(args[0])

        interactive = True
        exit = False
        script_args = []

        i = 1
        while i < len(args):
            arg = args[i]

            if arg.startswith('-') and arg != '-':
                if arg == '-v' or arg == '--version':
                    print "Pixie 0.1"
                    return 0
                elif arg == '-h' or arg == '--help':
                    print args[0] + " [<options>] [<file>]"
                    print "  -h, --help             print this help"
                    print "  -v, --version          print the version number"
                    print "  -e, --eval=<expr>      evaluate the given expression"
                    print "  -l, --load-path=<path> add <path> to pixie.stdlib/load-paths"
                    print "  -c, --compile=<file>   compile <path> to a .pxic file"
                    return 0
                elif arg == '-e' or arg == '--eval':
                    i += 1
                    if i < len(args):
                        expr = args[i]
                        run_with_stacklets.invoke([EvalFn(expr)])
                        return 0
                    else:
                        print "Expected argument for " + arg
                        return 1
                elif arg == '-l' or arg == '--load-path':
                    i += 1
                    if i < len(args):
                        path = args[i]
                        add_to_load_paths(path)
                    else:
                        print "Expected argument for " + arg
                        return 1

                elif arg == "-c" or arg == "--compile":
                    i += 1
                    if i < len(args):
                        path = args[i]
                        print "Compiling ", path
                        run_with_stacklets.invoke([CompileFileFn(path)])
                        exit = True
                    else:
                        print "Expected argument for " + arg
                        return 1
                else:
                    print "Unknown option " + arg
                    return 1
            else:
                interactive = False
                script_args = args[i:]
                break

            i += 1

        if not exit:
            if interactive:
                run_with_stacklets.invoke([ReplFn(args)])
            else:
                run_with_stacklets.invoke([BatchModeFn(script_args)])
    except WrappedException as we:
        print we._ex.__repr__()

    return 0

def add_to_load_paths(path):
    rt.reset_BANG_(LOAD_PATHS.deref(), rt.conj(rt.deref(LOAD_PATHS.deref()), rt.wrap(path)))


def init_load_path(self_path):
    if not path.isfile(self_path):
        self_path = find_in_path(self_path)
        assert self_path is not None
    if path.islink(self_path):
        self_path = os.readlink(self_path)
    self_path = dirname(rpath.rabspath(self_path))

    # runtime is not loaded yet, so we have to do it manually
    LOAD_PATHS.set_root(Atom(EMPTY_VECTOR.conj(rt.wrap(self_path))))
    # just for run_load_stdlib (global variables can't be assigned to)
    load_path.set_root(rt.wrap(self_path))

def dirname(path):
    return rpath.sep.join(path.split(rpath.sep)[0:-1])

def find_in_path(exe_name):
    paths = os.environ.get('PATH')
    paths = paths.split(path.pathsep)
    for p in paths:
        exe_path = path.join(p, exe_name)
        if path.isfile(exe_path):
            return exe_path
    return None

from rpython.rtyper.lltypesystem import lltype
from rpython.jit.metainterp import warmspot

def run_child(glob, loc):
    interp = loc['interp']
    graph = loc['graph']
    interp.malloc_check = False

    def returns_null(T, *args, **kwds):
        return lltype.nullptr(T)
    interp.heap.malloc_nonmovable = returns_null     # XXX

    from rpython.jit.backend.llgraph.runner import LLGraphCPU
    #LLtypeCPU.supports_floats = False     # for now
    apply_jit(interp, graph, LLGraphCPU)


def apply_jit(interp, graph, CPUClass):
    print 'warmspot.jittify_and_run() started...'
    policy = Policy()
    warmspot.jittify_and_run(interp, graph, [], policy=policy,
                             listops=True, CPUClass=CPUClass,
                             backendopt=True, inline=True)

def run_debug(argv):
    from rpython.rtyper.test.test_llinterp import get_interpreter

    # first annotate and rtype
    try:
        interp, graph = get_interpreter(entry_point, [], backendopt=False,
                                        #config=config,
                                        #type_system=config.translation.type_system,
                                        policy=Policy())
    except Exception, e:
        print '%s: %s' % (e.__class__, e)
        pdb.post_mortem(sys.exc_info()[2])
        raise

    # parent process loop: spawn a child, wait for the child to finish,
    # print a message, and restart
    #unixcheckpoint.restartable_point(auto='run')

    from rpython.jit.codewriter.codewriter import CodeWriter
    CodeWriter.debug = True
    run_child(globals(), locals())

import pixie.vm.rt as rt
rt.init()
#stacklet.global_state = stacklet.GlobalState()

def target(*args):
    import pixie.vm.rt as rt
    driver = args[0]
    driver.exe_name = "pixie-vm"
    rt.__config__ = args[0].config


    print "ARG INFO: ", args


    return entry_point, None

import rpython.config.translationoption
print rpython.config.translationoption.get_combined_translation_config()

if __name__ == "__main__":
    entry_point(sys.argv)
