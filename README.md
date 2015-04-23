# fopltlmon

fopltl is the implementation of a runtime verifiaction and measuring algorithm for specifiactions formulated in First-order Parametric LTL (FO-PLTL).
It processes input traces and computes measuring results.

Usage
-----

You can use fopltlmon as a command line tool. If you run `java -jar fopltlmon.jar --help`, it will show your options:

```
Usage: scopt [options] <formula>

  <formula>
        input formula as string
  -t <trace-file> | --trace <trace-file>
        path of trace file
  -o <out-file> | --out <out-file>
        output file for result
  -v <out-file> | --verbose <out-file>
        store number of automata/runs & memory usage after each step in file
  -w | --weak
        use weak acceptance for until and eventually
  --help
        prints this usage text
```


Syntax
------

The formula syntax corresponds to the usual LTL syntax with quantifiers `A` and `E`. An example specification looks like:

```
G ( A x:p. F<=k q(x) )
```

Trace files contain one event per line, e.g.

```
{p(10),q(0)}
{}
{q(10),p(25)}
```


Installing
----------

Either download the standalone ltlfo2mon.jar in the base directory, or run and build the project with sbt 0.12.x. 

License
-------

GPLv3 License.
