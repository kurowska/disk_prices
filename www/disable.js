$(document).ready(function(){
	
	disableButtons(1, 4, "check2", true); //Disable all
	
	$('.radio').click(function() {
	
	
	//Uncheck custom
        	if($('#tv1').prop('checked')){
			disableButtons(1, 4, "check2", false); //Enable all
			disableButtons(1, 2, "check2", true); //Disable first 2
		}
		
		if($('#tv2').prop('checked')){
			disableButtons(1, 4, "check2", false); //Enable all
			disableButtons(1, 2, "check2", true); //Disable 1 to 2
			disableButtons(4, 4, "check2", true); //and last
		}
		
		if($('#tv3').prop('checked')){
			disableButtons(1, 4, "check2", false); //Enable all
			disableButtons(1, 1, "check2", true); //Disable 1st
			disableButtons(3, 4, "check2", true); //and 3 to 4
		}
		
		if($('#tv4').prop('checked')){
			disableButtons(1, 4, "check2", false); //Enable all
			disableButtons(2, 4, "check2", true); //Disable 2 to 4
		}
		
		if($('#tv5').prop('checked')){
			disableButtons(1, 4, "check2", false); //Enable all
		}

		
	})
})

var disableButtons = function(from, to, id, dis) {
	for(i = from; i<= to; i++) {
		$('#' + id + "" + i).prop('disabled', dis);
	}
}