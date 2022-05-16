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
# A markdown Idris file

We hide the module declaration because it is **booooring**



We are however quite proud to be working with total functions only:

<code class="IdrisCode"><span class="IdrisKeyword">%default</span>&nbsp;<span class="IdrisKeyword">total</span><br />
</code>

Here is a failing block

<code class="IdrisCode"><span class="IdrisKeyword">failing</span>&nbsp;<span class="IdrisData">&quot;Can&apos;t&nbsp;find&nbsp;an&nbsp;implementation&nbsp;for&nbsp;FromString&nbsp;Nat&quot;</span><br />
&nbsp;&nbsp;<span class="IdrisFunction">t</span>&nbsp;<span class="IdrisKeyword">:</span>&nbsp;<span class="IdrisType">Nat</span><br />
&nbsp;&nbsp;<span class="IdrisFunction">t</span>&nbsp;<span class="IdrisKeyword">=</span>&nbsp;<span class="IdrisData">&quot;h&quot;</span><br />
</code>

And here is a successful definition

<code class="IdrisCode"><span class="IdrisFunction">main</span>&nbsp;<span class="IdrisKeyword">:</span>&nbsp;<span class="IdrisType">IO</span>&nbsp;<span class="IdrisType">()</span><br />
<span class="IdrisFunction">main</span>&nbsp;<span class="IdrisKeyword">=</span>&nbsp;<span class="IdrisFunction">putStrLn</span>&nbsp;<span class="IdrisData">&quot;Hello,&nbsp;but&nbsp;in&nbsp;Markdown&quot;</span><br />
</code>

