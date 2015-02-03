<apply template="base">

<ifLoggedIn>

	<apply template="_loggedin"/>


	<table class="calendar">

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
	<a href="/calendar/${previousyear}">Previous year</a>
	<b>--</b>
	<a href="/calendar/${previous}">Previous Month</a> 
	<b>--</b>
	<a href="/calendar/${next}">Next Month</a>
	<b>--</b>
	<a href="/calendar/${nextyear}">Next year</a>


	<br/>
	<br/>
	<i>Create a new event <a href="/event/${year}/${month}"> here</a></i>
	<br/>
	<br/>
	<h2>Events for <monthAsString/></h2>

	<events>
		<h4><day/> <monthAsString/></h4>
		name: <b><title/></b><br/>
		is recurring event: <recurs/> <br/>
		interval : <interval/><br/>
		
		<b>
			<delete>
				<a href="/calendar/delete/${id}">delete</a>
			</delete>
		</b>
		<hr/>
	</events>

	<br>
	<br>
	


</ifLoggedIn>
<ifLoggedOut>
	<h2>You cannot access this page!</h2>
	Please log in <a href="/">here </a>
</ifLoggedOut>



</apply>