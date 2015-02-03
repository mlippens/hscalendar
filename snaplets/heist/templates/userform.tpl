<form method="post" action="${postAction}" role="form">
  <div class="form-group">
    <label for="login">Username</label>
    <input type="text" size="30" name="login" id="login" placeholder="username" class="form-control">
  </div>
  <div class="form-group">
    <label for="password">Password</label>
    <input type="text" size="30" name="password" id="password" placeholder="password" class="form-control">
  </div>

  <input type="submit" class="btn btn-primary" value="${submitText}" />

</form>
