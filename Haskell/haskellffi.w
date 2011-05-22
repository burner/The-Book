>﻿{{Haskell minitoc|chapter=General Practices}}
<input type="hidden" value="d41d8cd98f00b204e9800998ecf8427e" name="wpAutoSummary" /><input type="hidden" value="0" name="oldid" /><textarea tabindex="1" accesskey="," id="wpTextbox1" cols="80" rows="25" style="" name="wpTextbox1">﻿{{Haskell minitoc|chapter=General Practices}}

Using Haskell is fine, but in the real world there is a large amount of useful libraries in other languages, especially C.
To use these libraries, and let C code use Haskell functions, there is the Foreign Function Interface (FFI).

== Calling C from Haskell ==

=== Marshalling (Type Conversion) ===
When using C functions, it is necessary to convert Haskell types to the appropriate C types.
These are available in the <code>Foreign.C.Types</code> module; some examples are given in the following table.
{| class="wikitable" style="border: 1; margin: auto"
|-
! Haskell
! Foreign.C.Types
! C
|-
| Double
| CDouble
| double
|-
| Char
| CUChar
| unsigned char
|-
| Int
| CLong
| long int
|}

The operation of converting Haskell types into C types is called '''marshalling''' (and the opposite, predictably, ''unmarshalling'').
For basic types this is quite straightforward: for floating-point one uses <code>realToFrac</code> (either way, as e.g. both <code>Double</code> and <code>CDouble</code> are instances of classes <code>Real</code> and <code>Fractional</code>), for integers <code>fromIntegral</code>, and so on.

{{Warning|If you are using GHC previous to 6.12.x, note that the <code>CLDouble</code> type does not really represent a <code>long double</code>, but is just a synonym for <code>CDouble</code>: ''never use it'', since it will lead to silent type errors if the C compiler does not also consider <code>long double</code> a synonym for <code>double</code>. Since 6.12.x <code>CLDouble</code> [http://hackage.haskell.org/trac/ghc/ticket/2793 has been removed], [http://hackage.haskell.org/trac/ghc/ticket/3353 pending proper implementation].}}

=== Calling a pure C function ===
A pure function implemented in C does not present significant trouble in Haskell.
The <code>sin</code> function of the C standard library is a fine example:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "math.h sin"
     c_sin :: CDouble -> CDouble
</source>
First, we specify a GHC extension for the FFI in the first line.
We then import the <code>Foreign</code> and <code>Foreign.C.Types</code> modules, the latter of which contains information about <code>CDouble</code>, the representation of double-precision floating-point numbers in C.

We then specify that we are ''import''ing a ''foreign'' function, with a call to C.
A "safety level" has to be specified with the keyword <code>safe</code> (the default) or <code>unsafe</code>.
In general, <code>unsafe</code> is more efficient, and <code>safe</code> is required only for C code that could call back a Haskell function.
Since that is a very particular case, it is actually quite safe to use the <code>unsafe</code> keyword in most cases.
Finally, we need to specify header and function name, separated by a space.

The Haskell function name is then given, in our case we use a standard <code>c_sin</code>, but it could have been anything. Note that the function signature must be correct—GHC will not check the C header to confirm that the function actually takes a <code>CDouble</code> and returns another, and writing a wrong one could have unpredictable results.

It is then possible to generate a wrapper around the function using <code>CDouble</code> so that it looks exactly like any Haskell function.
<source lang="haskell">
haskellSin :: Double -> Double
haskellSin = realToFrac . c_sin . realToFrac
</source>

Importing C's <code>sin</code> is simple because it is a pure function that takes a plain <code>double</code> as input and returns another as output: things will complicate with impure functions and pointers, which are ubiquitous in more complicated C libraries.

=== Impure C Functions ===
A classic impure C function is <code>rand</code>, for the generation of pseudo-random numbers.
Suppose you do not want to use Haskell's <code>System.Random.randomIO</code>, for example because you want to replicate exactly the series of pseudo-random numbers output by some C routine. Then, you could import it just like <code>sin</code> before:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "stdlib.h rand"
     c_rand :: CUInt -- Oops!
</source>

If you try this naïve implementation in GHCI, you will notice that <code>c_rand</code> is returning always the same value: 
 > c_rand
 1714636915
 > c_rand
 1714636915
indeed, we have told GHC that it is a pure function, and GHC sees no point in calculating twice the result of a pure function.
Note that GHC did not give any error or warning message.

In order to make GHC understand this is no pure function, we have to use the [[Haskell/Understanding monads/IO|IO monad]]:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "stdlib.h rand"
     c_rand :: IO CUInt

foreign import ccall "stdlib.h srand"
     c_srand :: CUInt -> IO ()
</source>

Here, we also imported the <code>srand</code> function, to be able to seed the C pseudo-random generator.
 > c_rand
 1957747793
 > c_rand
 424238335
 > c_srand 0
 > c_rand
 1804289383
 > c_srand 0
 > c_rand
 1804289383

=== Working with C Pointers ===
The most useful C functions are often those that do complicated calculations with several parameters, and with increasing complexity the need of returning control codes arises.
This means that a typical paradigm of C libraries is to give pointers of allocated memory as "targets" in which the results may be written, while the function itself returns an integer value (typically, if 0, computation was successful, otherwise there was a problem specified by the number).
Another possibility is that the function will return a pointer to a structure (possibly defined in the implementation, and therefore unavailable to us).

As a pedagogical example, we consider [http://www.gnu.org/software/gsl/manual/html_node/Elementary-Functions.html the <code>gsl_frexp</code> function] of the [http://en.wikipedia.org/wiki/GNU_Scientific_Library GNU Scientific Library], a freely available library for scientific computation.
It is a simple C function with prototype:
<source lang="c">
double gsl_frexp (double x, int * e)
</source>

The function takes a <code>double</code> ''x'', and it returns its normalised fraction ''f'' and integer exponent ''e'' so that:
:<math> x = f \times 2^e \qquad e \in \mathbb{Z}, \quad 0.5 \leq f < 1</math>

We interface this C function into Haskell with the following code:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.Ptr
import Foreign.C.Types

foreign import ccall unsafe "gsl/gsl_math.h gsl_frexp"
     gsl_frexp :: CDouble -> Ptr CInt -> IO CDouble
</source>

The new part is <code>Ptr</code>, which can be used with any instance of the <code>Storable</code> class, among which all C types, but also several Haskell types.

Notice how the result of the <code>gsl_frexp</code> function is in the <code>IO</code> monad.
This is typical when working with pointers, be they used for input or output (as in this case); we will see shortly what would happen had we used a simple <code>CDouble</code> for the function.

The <code>frexp</code> function is implemented in pure Haskell code as follows:
<source lang="haskell">
frexp :: Double -> (Double, Int)
frexp x = unsafePerformIO $
    alloca $ \expptr -> do
        f <- gsl_frexp (realToFrac x) expptr
        e <- peek expptr
        return (realToFrac f, fromIntegral e)
</source>

We know that, memory management details aside, the function is pure: that's why the signature returns a tuple with ''f'' and ''e'' outside of the <code>IO</code> monad.
Yet, ''f'' is provided ''inside'' of it: to extract it, we use the function ''unsafePerformIO'', which extracts values from the <code>IO</code> monad: obviously, it is legitimate to use it only when we ''know'' the function is pure, and we can allow GHC to optimise accordingly.

To allocate pointers, we use the <code>alloca</code> function, which also takes responsibility for freeing memory.
As an argument, <code>alloca</code> takes a function of type <code>Ptr a -> IO b</code>, and returns the <code>IO b</code>.
In practice, this translates to the following usage pattern with &lambda; functions:
<source lang="haskell">
... alloca $ \pointer -> do
        c_function( argument, pointer )
        result <- peek pointer
        return result
</source>

The pattern can easily be nested if several pointers are required:
<source lang="haskell">
... alloca $ \firstPointer ->
        alloca $ \secondPointer -> do
            c_function( argument, firstPointer, secondPointer )
            first  <- peek firstPointer
            second <- peek secondPointer
            return (first, second)
</source>

Back to our <code>frexp</code> function: in the &lambda; function that is the argument to <code>alloca</code>, the function is evaluated and the pointer is read immediately afterwards with <code>peek</code>.
Here we can understand why we wanted the imported C function <code>gsl_frexp</code> to return a value in the <code>IO</code> monad: if GHC could decide when to calculate the quantity ''f'', it would likely decide not to do it until it is necessary: that is at the last line when <code>return</code> uses it, and ''after'' ''e'' has been read from an allocated, but yet uninitialised memory address, which will contain random data. In short, we want <code>gsl_frexp</code> to return a monadic value because we want to determine the sequence of computations ourselves.

If some other function had required a pointer to ''provide input'' instead of storing output, one would have used the similar <code>poke</code> function to set the pointed value, obviously ''before'' evaluating the function:
<source lang="haskell">
... alloca $ \inputPointer ->
        alloca $ \outputPointer -> do
            poke inputPointer value
            c_function( argument, inputPointer, outputPointer )
            result <- peek outputPointer
            return result
</source>

In the final line, the results are arranged in a tuple and returned, after having been converted from C types.

To test the function, remember to link GHC to the GSL; in GHCI, do:
 $ ghci frexp.hs -lgsl

(Note that most systems do not come with the GSL preinstalled, and you may have to download and install its development packages.)

=== Working with C Structures ===
Very often data are returned by C functions in form of <code>struct</code>s or pointers to these.
In some rare cases, these structures are returned directly, but more often they are returned as pointers; the return value is most often an <code>int</code> that indicates the correctness of execution.

We will consider another GSL function, [http://www.gnu.org/software/gsl/manual/html_node/Regular-Cylindrical-Bessel-Functions.html <code>gsl_sf_bessel_Jn_e</code>].
This function provides the regular cylindrical Bessel function for a given order ''n'', and returns the result as a <code>gsl_sf_result</code> structure pointer.
The structure contains two <code>double</code>s, one for the result and one for the error.
The integer error code returned by the function can be transformed in a C string by the function <code>gsl_strerror</code>.
The signature of the Haskell function we are looking for is therefore:
<source lang="haskell">
BesselJn :: Int -> Double -> Either String (Double, Double)
</source>
where the first argument is the order of the cylindrical Bessel function, the second is the function's argument, and the returned value is either an error message or a tuple with result and margin of error.

==== Making a New Instance of the <code>Storable</code> class ====
In order to allocate and read a pointer to a <code>gsl_sf_result</code> structure, it is necessary to make it an instance of the <code>Storable</code> class.

In order to do that, it is useful to use the <code>hsc2hs</code> program: we create first a <tt>Bessel.hsc</tt> file, with a mixed syntax of Haskell and C macros, which is later expanded into Haskell by the command:
 $ hsc2hs Bessel.hsc
After that, we simply load the <tt>Bessel.hs</tt> file in GHC.

This is the first part of file <tt>Bessel.hsc</tt>:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface #-}

module Bessel (besselJn) where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include <gsl/gsl_sf_result.h>

data GslSfResult = GslSfResult { gsl_value :: CDouble, gsl_error :: CDouble }

instance Storable GslSfResult where
    sizeOf    _ = (#size gsl_sf_result)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        value <- (#peek gsl_sf_result, val) ptr
        error <- (#peek gsl_sf_result, err) ptr
        return  GslSfResult { gsl_value = value, gsl_error = error }
    poke ptr (GslSfResult value error) = do
        (#poke gsl_sf_result, val) ptr value
        (#poke gsl_sf_result, err) ptr error
</source>

We use the <code>#include</code> directive to make sure <code>hsc2hs</code> knows where to find information about <code>gsl_sf_result</code>.
We then define a Haskell data structure mirroring the GSL's, with two <code>CDouble</code>s: this is the class we make an instance of <code>Storable</code>.
Strictly, we need only <code>sizeOf</code>, <code>alignment</code> and <code>peek</code> for this example; <code>poke</code> is added for completeness.

* <code>sizeOf</code> is obviously fundamental to the allocation process, and is calculated by <code>hsc2hs</code> with the <code>#size</code> macro.
* <code>alignment</code> is the size in byte of the [http://en.wikipedia.org/wiki/Data_structure_alignment data structure alignment]. In general, it should be the largest <code>alignment</code> of the elements of the structure; in our case, since the two elements are the same, we simply use <code>CDouble</code>'s. The value of the argument to <code>alignment</code> is inconsequential, what is important is the type of the argument.
* <code>peek</code> is implemented using a <code>do</code>-block and the <code>#peek</code> macros, as shown. <code>val</code> and <code>err</code> are the names used for the structure fields in the GSL source code.
* Similarly, <code>poke</code> is implemented with the <code>#poke</code> macro.

==== Importing the C Functions ====
<source lang="haskell">
foreign import ccall unsafe "gsl/gsl_bessel.h gsl_sf_bessel_Jn_e"
     c_besselJn :: CInt -> CDouble -> Ptr GslSfResult -> IO CInt

foreign import ccall unsafe "gsl/gsl_errno.h gsl_set_error_handler_off"
     c_deactivate_gsl_error_handler :: IO ()

foreign import ccall unsafe "gsl/gsl_errno.h gsl_strerror"
     c_error_string :: CInt -> IO CString
</source>

We import several functions from the GSL libraries: first, the Bessel function itself, which will do the actual work. Then, we need a particular function, <code>gsl_set_error_handler_off</code>, because the default GSL error handler will simply crash the program, even if called by Haskell: we, instead, plan to deal with errors ourselves.
The last function is the GSL-wide interpreter that translates error codes in human-readable C strings.

==== Implementing the Bessel Function ====
Finally, we can implement the Haskell version of the GSL cylindrical Bessel function of order ''n''.
<source lang="haskell">
besselJn :: Int -> Double -> Either String (Double, Double)
besselJn n x = unsafePerformIO $
    alloca $ \gslSfPtr -> do
        c_deactivate_gsl_error_handler
        status <- c_besselJn (fromIntegral n) (realToFrac x) gslSfPtr
        if status == 0
            then do
                GslSfResult val err <- peek gslSfPtr
                return $ Right (realToFrac val, realToFrac err)
            else do
                error <- c_error_string status
                error_message <- peekCString error
                return $ Left ("GSL error: "++error_message)
</source>

Again, we use <code>unsafePerformIO</code> because the function is pure, even though its nuts-and-bolts implementation is not.
After allocating a pointer to a GSL result structure, we deactivate the GSL error handler to avoid crashes in case something goes wrong, and finally we can call the GSL function.
At this point, if the <code>status</code> returned by the function is 0, we unmarshal the result and return it as a tuple.
Otherwise, we call the GSL error-string function, and pass the error as a <code>Left</code> result instead.

==== Examples ====
Once we are finished writing the <code>Bessel.hsc</code> function, we have to convert it to proper Haskell and load the produced file:
 $ hsc2hs Bessel.hsc
 $ ghci Bessel.hs -lgsl

We can then call the Bessel function with several values:
 > besselJn 0 10
 Right (-0.2459357644513483,1.8116861737200453e-16)
 > besselJn 1 0
 Right (0.0,0.0)
 > besselJn 1000 2
 Left "GSL error: underflow"

=== Advanced Topics ===
This section contains an advanced example with some more complex features of the FFI.
We will import into Haskell one of the more complicated functions of the GSL, the one used to calculate [http://www.gnu.org/software/gsl/manual/html_node/QAG-adaptive-integration.html the integral of a function between two given points with an adaptive Gauss-Kronrod algorithm].
The GSL function is <code>gsl_integration_qag</code>.

This example will illustrate function pointers, export of Haskell functions to C routines, enumerations, and handling pointers of unknown structures.

==== Available C Functions and Structures ====
The GSL has three functions which are necessary to integrate a given function with the considered method:
<source lang="c">
gsl_integration_workspace * gsl_integration_workspace_alloc (size_t n);
void gsl_integration_workspace_free (gsl_integration_workspace * w);
int gsl_integration_qag (const gsl_function * f, double a, double b, 
                         double epsabs, double epsrel, size_t limit, 
                         int key, gsl_integration_workspace * workspace, 
                         double * result, double * abserr);
</source>
The first two deal with allocation and deallocation of a "workspace" structure of which we know nothing (we just pass a pointer around).
The actual work is done by the last function, which requires a pointer to a workspace.

To provide functions, the GSL specifies an appropriate structure for C:
<source lang="c">
struct gsl_function
{
  double (* function) (double x, void * params);
  void * params;
};
</source>
The reason for the <code>void</code> pointer is that it is not possible to define &lambda; functions in C: parameters are therefore passed along with a parameter of unknown type.
In Haskell, we do not need the <code>params</code> element, and will consistently ignore it.

==== Imports and Inclusions ====
We start our <tt>qag.hsc</tt> file with the following:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Qag ( qag,
             gauss15,
             gauss21,
             gauss31,
             gauss41,
             gauss51,
             gauss61 ) where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

#include <gsl/gsl_math.h>
#include <gsl/gsl_integration.h>

foreign import ccall unsafe "gsl/gsl_errno.h gsl_strerror"
     c_error_string :: CInt -> IO CString

foreign import ccall unsafe "gsl/gsl_errno.h gsl_set_error_handler_off"
    c_deactivate_gsl_error_handler :: IO ()
</source>
We declare the <code>EmptyDataDecls</code> pragma, which we will use later for the <code>Workspace</code> data type.
Since this file will have a good number of functions that should not be available to the outside world, we also declare it a module and export only the final function <code>qag</code> and the <code>gauss</code>- flags.
We also include the relevant C headers of the GSL.
The import of C functions for error messages and deactivation of the error handler was described before.

==== Enumerations ====
One of the arguments of <code>gsl_integration_qag</code> is <code>key</code>, an integer value that can have values from 1 to 6 and indicates the integration rule.
GSL defines a macro for each value, but in Haskell it is more appropriate to define a type, which we call <code>IntegrationRule</code>.
Also, to have its values automatically defined by <tt>hsc2hs</tt>, we can use the <code>enum</code> macro:
<source lang="haskell">
newtype IntegrationRule = IntegrationRule { rule :: CInt }
#{enum IntegrationRule, IntegrationRule,
    gauss15 = GSL_INTEG_GAUSS15,
    gauss21 = GSL_INTEG_GAUSS21,
    gauss31 = GSL_INTEG_GAUSS31,
    gauss41 = GSL_INTEG_GAUSS41,
    gauss51 = GSL_INTEG_GAUSS51,
    gauss61 = GSL_INTEG_GAUSS61
  }
</source>
<tt>hsc2hs</tt> will search the headers for the macros and give our variables the correct values.
The variables cannot be modified and are essentially constant flags.
Since we did not export the <code>IntegrationRule</code> constructor in the module declaration, but only the <code>gauss</code> flags, it is impossible for a user to even construct an invalid value. One thing less to worry about!

==== Haskell Function Target ====
We can now write down the signature of the function we desire:
<source lang="haskell">
qag :: IntegrationRule                 -- Algorithm type
    -> Int                             -- Step limit
    -> Double                          -- Absolute tolerance
    -> Double                          -- Relative tolerance
    -> (Double -> Double)              -- Function to integrate
    -> Double                          -- Integration interval start
    -> Double                          -- Integration interval end
    -> Either String (Double, Double)  -- Result and (absolute) error estimate
</source>
Note how the order of arguments is different from the C version: indeed, since C does not have the possibility of partial application, the ordering criteria are different than in Haskell.

As in the previous example, we indicate errors with a <code>Either String (Double, Double)</code> result.

==== Passing Haskell Functions to the C Algorithm ====
<source lang="haskell">
type CFunction = CDouble -> Ptr () -> CDouble

data GslFunction = GslFunction (FunPtr CFunction) (Ptr ())
instance Storable GslFunction where
    sizeOf    _ = (#size gsl_function)
    alignment _ = alignment (undefined :: Ptr ())
    peek ptr = do
        function <- (#peek gsl_function, function) ptr
        return $ GslFunction function nullPtr
    poke ptr (GslFunction fun nullPtr) = do
        (#poke gsl_function, function) ptr fun

makeCfunction :: (Double -> Double) -> (CDouble -> Ptr () -> CDouble)
makeCfunction f = \x voidpointer -> realToFrac $ f (realToFrac x)

foreign import ccall "wrapper"
    makeFunPtr :: CFunction -> IO (FunPtr CFunction)
</source>

We define a shorthand type, <code>CFunction</code>, for readability. Note that the <code>void</code> pointer has been translated to a <code>Ptr ()</code>, since we have no intention of using it.
Then it is the turn of the <code>gsl_function</code> structure: no surprises here. Note that the <code>void</code> pointer is always assumed to be null, both in <code>peek</code> and in <code>poke</code>, and is never really read nor written.

To make a Haskell <code>Double -> Double</code> function available to the C algorithm, we make two steps: first, we re-organise the arguments using a &lambda; function in <code>makeCfunction</code>; then, in <code>makeFunPtr</code>, we take the function with reordered arguments and produce a function pointer that we can pass on to <code>poke</code>, so we can construct the <code>GslFunction</code> data structure.

==== Handling Unknown Structures ====
<source lang="haskell">
data Workspace
foreign import ccall unsafe "gsl/gsl_integration.h gsl_integration_workspace_alloc"
    c_qag_alloc :: CSize -> IO (Ptr Workspace)
foreign import ccall unsafe "gsl/gsl_integration.h gsl_integration_workspace_free"
    c_qag_free  :: Ptr Workspace -> IO ()

foreign import ccall safe "gsl/gsl_integration.h gsl_integration_qag"
    c_qag :: Ptr GslFunction -- Allocated GSL function structure
          -> CDouble -- Start interval
          -> CDouble -- End interval
          -> CDouble -- Absolute tolerance
          -> CDouble -- Relative tolerance
          -> CSize   -- Maximum number of subintervals
          -> CInt    -- Type of Gauss-Kronrod rule
          -> Ptr Workspace -- GSL integration workspace
          -> Ptr CDouble -- Result
          -> Ptr CDouble -- Computation error
          -> IO CInt -- Exit code
</source>

The reason we imported the <code>EmptyDataDecls</code> pragma is this: we are declaring the data structure <code>Workspace</code> without providing any constructor.
This is a way to make sure it will always be handled as a pointer, and never actually instantiated.

Otherwise, we normally import the allocating and deallocating routines.
We can now import the integration function, since we have all the required pieces (<code>GslFunction</code> and <code>Workspace</code>).

==== The Complete Function ====
It is now possible to implement a function with the same functionality as the GSL's QAG algorithm.

<source lang="haskell">
qag gauss steps abstol reltol f a b = unsafePerformIO $ do
    c_deactivate_gsl_error_handler
    workspacePtr <- c_qag_alloc (fromIntegral steps)
    if workspacePtr == nullPtr
        then
            return $ Left "GSL could not allocate workspace"
        else do
            fPtr <- makeFunPtr $ makeCfunction f
            alloca $ \gsl_f -> do
                poke gsl_f (GslFunction fPtr nullPtr)
                alloca $ \resultPtr -> do
                    alloca $ \errorPtr -> do
                        status <- c_qag gsl_f
                                        (realToFrac a)
                                        (realToFrac b)
                                        (realToFrac abstol)
                                        (realToFrac reltol)
                                        (fromIntegral steps)
                                        (rule gauss)
                                        workspacePtr
                                        resultPtr
                                        errorPtr
                        c_qag_free workspacePtr
                        freeHaskellFunPtr fPtr
                        if status /= 0
                            then do
                                c_errormsg <- c_error_string status
                                errormsg   <- peekCString c_errormsg
                                return $ Left errormsg
                            else do
                                c_result <- peek resultPtr
                                c_error  <- peek  errorPtr
                                let result = realToFrac c_result
                                let error  = realToFrac c_error
                                return $ Right (result, error)
</source>

First and foremost, we deactivate the GSL error handler, that would crash the program instead of letting us report the error.

We then proceed to allocate the workspace; notice that, if the returned pointer is null, there was an error (typically, too large size) that has to be reported.

If the workspace was allocated correctly, we convert the given function to a function pointer and allocate the <code>GslFunction</code> struct, in which we place the function pointer.
Allocating memory for the result and its error margin is the last thing before calling the main routine.

After calling, we have to do some housekeeping and free the memory allocated by the workspace and the function pointer. Note that it would be possible to skip the bookkeeping using <code>ForeignPtr</code>, but the work required to get it to work is more than the effort to remember one line of cleanup.

We then proceed to check the return value and return the result, as was done for the Bessel function.

=== Self-Deallocating Pointers ===
In the previous example, we manually handled the deallocation of the GSL integration workspace, a data structure we know nothing about, by calling its C deallocation function.
It happens that the same workspace is used in several integration routines, which we may want to import in Haskell.

Instead of replicating the same allocation/deallocation code each time, which could lead to memory leaks when someone forgets the deallocation part, we can provide a sort of "smart pointer", which will deallocate the memory when it is not needed any more.
This is called <code>ForeignPtr</code> (do not confuse with <code>Foreign.Ptr</code>: this one's qualified name is actually <code>Foreign.ForeignPtr</code>!).
The function handling the deallocation is called the ''finalizer''.

In this section we will write a simple module to allocate GSL workspaces and provide them as appropriately configured <code>ForeignPtr</code>s, so that users do not have to worry about deallocation.

The module, written in file <tt>GSLWorkspace.hs</tt>, is as follows:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module GSLWorkSpace (Workspace, createWorkspace) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

data Workspace
foreign import ccall unsafe "gsl/gsl_integration.h gsl_integration_workspace_alloc"
    c_ws_alloc :: CSize -> IO (Ptr Workspace)
foreign import ccall unsafe "gsl/gsl_integration.h &gsl_integration_workspace_free"
    c_ws_free  :: FunPtr( Ptr Workspace -> IO () )

createWorkspace :: CSize -> IO (Maybe (ForeignPtr Workspace) )
createWorkspace size = do
    ptr <- c_ws_alloc size
    if ptr /= nullPtr
        then do
           foreignPtr <- newForeignPtr c_ws_free ptr
           return $ Just foreignPtr
        else
           return Nothing
</source>

We first declare our empty data structure <code>Workspace</code>, just like we did in the previous section.

The <code>gsl_integration_workspace_alloc</code> and <code>gsl_integration_workspace_free</code> functions will no longer be needed in any other file: here, note that the deallocation function is called with an ampersand ("&"), because we do not actually want the function, but rather a ''pointer'' to it to set as a finalizer.

The workspace creation function returns a IO (Maybe) value, because there is still the possibility that allocation is unsuccessful and the null pointer is returned. The GSL does not specify what happens if the deallocation function is called on the null pointer, so for safety we do not set a finalizer in that case and return <code>IO Nothing</code>; the user code will then have to check for "<code>Just</code>-ness" of the returned value.

If the pointer produced by the allocation function is non-null, we build a foreign pointer with the deallocation function, inject into the <code>Maybe</code> and then the <code>IO</code> monad. That's it, the foreign pointer is ready for use!

{{Warning|This function requires object code to be compiled, so if you load this module with GHCI (which is an interpreter) you must indicate it:
 $ ghci GSLWorkSpace.hs -fobject-code
Or, from within GHCI:
 > :set -fobject-code
 > :load GSLWorkSpace.hs
}}

The <tt>qag.hsc</tt> file must now be modified to use the new module; the parts that change are:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface #-}

-- [...]

import GSLWorkSpace

import Data.Maybe(isNothing, fromJust)

-- [...]

qag gauss steps abstol reltol f a b = unsafePerformIO $ do
    c_deactivate_gsl_error_handler
    ws <- createWorkspace (fromIntegral steps)
    if isNothing ws
        then
            return $ Left "GSL could not allocate workspace"
        else do
            withForeignPtr (fromJust ws) $ \workspacePtr -> do

-- [...]
</source>

Obviously, we do not need the <code>EmptyDataDecls</code> extension here any more; instead we import the <code>GSLWorkSpace</code> module, and also a couple of nice-to-have functions from <code>Data.Maybe</code>.
We also remove the foreign declarations of the workspace allocation and deallocation functions.

The most important difference is in the main function, where we (try to) allocate a workspace <code>ws</code>, test for its <code>Just</code>ness, and if everything is fine we use the <code>withForeignPtr</code> function to extract the workspace pointer. Everything else is the same.

== Calling Haskell from C ==
Sometimes it is also convenient to call Haskell from C, in order to take advantage of some of Haskell's features which are tedious to implement in C, such as lazy evaluation.

We will consider a typical Haskell example, Fibonacci numbers.
These are produced in an elegant, haskellian one-liner as:
<source lang="haskell">
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
</source>

Our task is to export the ability to calculate Fibonacci numbers from Haskell to C.
However, in Haskell, we typically use the <code>Integer</code> type, which is unbounded: this cannot be exported to C, since there is no such corresponding type.
To provide a larger range of outputs, we specify that the C function shall output, whenever the result is beyond the bounds of its integer type, an approximation in floating-point.
If the result is also beyond the range of floating-point, the computation will fail.
The status of the result (whether it can be represented as a C integer, a floating-point type or not at all) is signalled by the status integer returned by the function.
Its desired signature is therefore:
<source lang="c">
int fib( int index, unsigned long long* result, double* approx )
</source>

=== Haskell Source ===
The Haskell source code for file <tt>fibonacci.hs</tt> is:
<source lang="haskell">
{-# LANGUAGE ForeignFunctionInterface #-}

module Fibonacci where

import Foreign
import Foreign.C.Types

fibonacci :: (Integral a) => [a]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

foreign export ccall fibonacci_c :: CInt -> Ptr CULLong -> Ptr CDouble -> IO CInt
fibonacci_c :: CInt -> Ptr CULLong -> Ptr CDouble -> IO CInt
fibonacci_c n intPtr dblPtr
    | badInt && badDouble = return 2
    | badInt              = do
        poke dblPtr dbl_result
        return 1
    | otherwise           = do
        poke intPtr (fromIntegral result)
        poke dblPtr dbl_result
        return 0
    where
    result     = fibonacci !! (fromIntegral n)
    dbl_result = realToFrac result
    badInt     = result > toInteger (maxBound :: CULLong)
    badDouble  = isInfinite dbl_result
</source>

When exporting, we need to wrap our functions in a module (it is a good habit anyway).
We have already seen the Fibonacci infinite list, so let's focus on the exported function: it takes an argument, two pointers to the target <code>unsigned long long</code> and <code>double</code>, and returns the status in the <code>IO</code> monad (since writing on pointers is a side effect).

The function is implemented with input guards, defined in the <code>where</code> clause at the bottom. A successful computation will return 0, a partially successful 1 (in which we still can use the floating-point value as an approximation), and a completely unsuccessful one will return 2.

Note that the function does not call <code>alloca</code>, since the pointers are assumed to have been already allocated by the calling C function.

The Haskell code can then be compiled with GHC:
 ghc -c fibonacci.hs

=== C Source ===
The compilation of <tt>fibonacci.hs</tt> has spawned several files, among which <tt>fibonacci_stub.h</tt>, which we include in our C code in file <tt>fib.c</tt>:
<source lang="c">
#include <stdio.h>
#include <stdlib.h>
#include "fibonacci_stub.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <number>\n", argv[0]);
        return 2;
    }

    hs_init(&argc, &argv);

    const int arg = atoi(argv[1]);
    unsigned long long res;
    double approx;
    const int status = fibonacci_c(arg, &res, &approx);

    hs_exit();
    switch (status) {
    case 0:
        printf("F_%d: %llu\n", arg, res);
        break;
    case 1:
        printf("Error: result is out of bounds\n");
        printf("Floating-point approximation: %e\n", approx);
        break;
    case 2:
        printf("Error: result is out of bounds\n");
        printf("Floating-point approximation is infinite\n");
        break;
    default:
        printf("Unknown error: %d\n", status);
    }

    return status;
}
</source>

The notable thing is that we need to initialise the Haskell environment with <code>hs_init</code>, which we call passing it the command-line arguments of main; we also have to shut Haskell down with  <code>hs_exit()</code> when we are done.
The rest is fairly standard C code for allocation and error handling. 

Note that you have to compile the C code ''with GHC'', not your C compiler!
 ghc fib.c fibonacci.o fibonacci_stub.o -o fib

You can then proceed to test the algorithm:
 ./fib 42
 F_42: 267914296
 $ ./fib 666
 Error: result is out of bounds
 Floating-point approximation: 6.859357e+138
 $ ./fib 1492
 Error: result is out of bounds
 Floating-point approximation is infinite
 ./fib -1
 fib: Prelude.(!!): negative index


{{Haskell navigation|chapter=General Practices}}

{{BookCat}}
