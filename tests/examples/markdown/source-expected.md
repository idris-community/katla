<style>
.IdrisData {
  color: darkred
}
.IdrisType {
  color: blue
}
.IdrisBound {
  color: black
}
.IdrisFunction {
  color: darkgreen
}
.IdrisKeyword {
  text-decoration: underline;
}
.IdrisComment {
  color: #b22222
}
.IdrisNamespace {
  font-style: italic;
  color: black
}
.IdrisPostulate {
  font-weight: bold;
  color: red
}
.IdrisModule {
  font-style: italic;
  color: black
}
.IdrisCode {
  display: block;
  background-color: whitesmoke;
}
</style>
# Tutorial: writing an Idris2 blog post

Using [katla](https://github.com/idris-community/katla)'s markdown backend
we can produce documents containing semantically highlighted Idris2 code.

The file you are currently reading is the rendered version of a literate
markdown/idris2 file. It is called `Source.md` and contains fenced `idris`
blocks. For instance the following code block declares the `Source` module.

<code class="IdrisCode">
<span class="IdrisKeyword">module</span>&nbsp;<span class="IdrisModule">Source</span><br />
</code>

It is easy to hide uninteresting code blocks. E.g. the following line contains
a code block importing `Data.String` which we have purposefully hidden by passing
the `hide` attribute to the `idris` fence.

But we decide to proudly display the fact all our definitions are total by default.
<code class="IdrisCode">
<span class="IdrisKeyword">%default</span>&nbsp;<span class="IdrisKeyword">total</span><br />
</code>

We can make use of all of the language's feature. E.g. we can write `failing` blocks
to illustrate invalid code. Because we have turned the totality checker on, we have
to write obviously terminating functions. The following function is for instance
rejected.

<code class="IdrisCode">
<span class="IdrisKeyword">failing</span>&nbsp;<span class="IdrisData">&quot;non\_structural\_product&nbsp;is&nbsp;not&nbsp;total,&nbsp;possibly&nbsp;not&nbsp;terminating&quot;</span><br />
<br />
&nbsp;&nbsp;<span class="IdrisFunction">non\_structural\_product</span>&nbsp;<span class="IdrisKeyword">:</span>&nbsp;<span class="IdrisType">List</span>&nbsp;<span class="IdrisType">Nat</span>&nbsp;<span class="IdrisKeyword">-&gt;</span>&nbsp;<span class="IdrisType">Nat</span><br />
&nbsp;&nbsp;<span class="IdrisFunction">non\_structural\_product</span>&nbsp;<span class="IdrisData">[]</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="IdrisKeyword">=</span>&nbsp;<span class="IdrisData">1</span><br />
&nbsp;&nbsp;<span class="IdrisFunction">non\_structural\_product</span>&nbsp;<span class="IdrisData">[</span><span class="IdrisBound">x</span><span class="IdrisData">]</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="IdrisKeyword">=</span>&nbsp;<span class="IdrisBound">x</span><br />
&nbsp;&nbsp;<span class="IdrisFunction">non\_structural\_product</span>&nbsp;<span class="IdrisKeyword">(</span><span class="IdrisBound">x</span><span class="IdrisData">::</span><span class="IdrisBound">y</span><span class="IdrisData">::</span><span class="IdrisBound">xs</span><span class="IdrisKeyword">)</span>&nbsp;<span class="IdrisKeyword">=</span>&nbsp;<span class="IdrisFunction">non\_structural\_product</span>&nbsp;<span class="IdrisKeyword">(</span><span class="IdrisBound">x</span><span class="IdrisFunction">\*</span><span class="IdrisBound">y</span><span class="IdrisData">::</span><span class="IdrisBound">xs</span><span class="IdrisKeyword">)</span><br />
</code>

And here is a successful definition (which demonstrates that we have indeed imported
`Data.String` in a hidden block and thus have access to `unwords`).
<code class="IdrisCode">
<span class="IdrisFunction">main</span>&nbsp;<span class="IdrisKeyword">:</span>&nbsp;<span class="IdrisType">IO</span>&nbsp;<span class="IdrisType">()</span><br />
<span class="IdrisFunction">main</span>&nbsp;<span class="IdrisKeyword">=</span>&nbsp;<span class="IdrisFunction">putStrLn</span><br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\$&nbsp;<span class="IdrisKeyword">(\\</span><span class="IdrisBound">x</span>&nbsp;<span class="IdrisKeyword">=&gt;</span>&nbsp;<span class="IdrisBound">x</span><span class="IdrisKeyword">)</span><br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\$&nbsp;<span class="IdrisFunction">unwords</span><br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="IdrisData">[</span>&nbsp;<span class="IdrisData">&quot;Hello,&quot;</span><br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="IdrisData">,</span>&nbsp;<span class="IdrisData">&quot;from&quot;</span><br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="IdrisData">,</span>&nbsp;<span class="IdrisData">&quot;the&quot;</span><br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="IdrisData">,</span>&nbsp;<span class="IdrisData">&quot;Markdown&quot;</span><br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="IdrisData">,</span>&nbsp;<span class="IdrisData">&quot;mode&quot;</span><br />
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="IdrisData">]</span><br />
</code>

