<apply template="base">
<ifLoggedIn>
	<apply template="_loggedin"/>
	
	<h2>Create event</h2>

	<table class="minicalendar">

		  <calendar>
	      <caption><monthAsString/> - <year/></caption>

	      <col class="weekday" span="5">
	      <col class="weekend" span="2">
	      <thead>
	        <tr>    
	          <th>Mon</th>
	          <th>Tue</th>
	          <th>Wed</th>
	          <th>Thu</th>
	          <th>Fri</th>
	          <th>Sat</th>
	          <th>Sun</th>
	        </tr>
	      </thead>
	      <tbody>
	      	<rows>
	      		
		      	<tr>
		      		<row>
						<td><value/></td>
					</row>
				</tr>
	      	</rows>
	      </tbody>
	      </calendar>
	</table>
	<a href="/event/${previousyear}">Previous year</a>
	<b>--</b>
	<a href="/event/${previous}">Previous Month</a> 
	<b>--</b>
	<a href="/event/${next}">Next Month</a>
	<b>--</b>
	<a href="/event/${nextyear}">Next year</a>

	<br/>
	<br/>

	<form role="form" method="post">
		<input name="day" type="hidden" value="1" />    
		<input name="year" type="hidden" value="${year}"/>
		<input name="month" type="hidden" value="${month}"/>

		<div class="form-group">
			<label for="eventName">Event name</label>
			<input type="text" class="form-control" name="eventName" id="eventName" placeholder="event name"/>
		</div>
		<div class="form-group">
			<label for="recurring">Does this event recur?</label>
			<select class="form-control" name="recurring">
				<option value="0">non recurring</option>
				<option value="1">daily</option>
				<option value="2">monthly</option>
				<option value="3">yearly</option>
			</select>
		</div>

		<input type="submit" class="btn btn-primary" value="Submit"/>

	</form>
</ifLoggedIn>
<ifLoggedOut>
	<h2>Please log in first <a href="/"> here </a></h2>
</ifLoggedOut>


</apply>