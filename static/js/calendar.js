$(function(){
	$('.minicalendar td:contains("1"):first').addClass('selected');
	$('.minicalendar td').on('click',function(){
		if(!$(this).hasClass('nonmonth')){
			$(this).closest('table.minicalendar').find('.selected').removeClass('selected');
	   		$(this).addClass('selected');
	   		$('form input[name="day"]').val($(this).html());	
		}
	});
});