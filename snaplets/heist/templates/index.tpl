<apply template="base">

  <ifLoggedIn>
    <h2>Index</h2>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
