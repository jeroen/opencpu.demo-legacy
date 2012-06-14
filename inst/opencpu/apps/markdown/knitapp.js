$(document).ready(function() {
  // Handler for .ready() called.
	var HEADERS;
	var LASTREPORT;
	
	var uploadhtml = function(){
		var timestamp = '<!--generated on ' + new Date() + " -->\n"
		var reportbase = btoa(timestamp + HEADERS + "\n" + LASTREPORT + "</div></body></html>");

		$.ajax({
			type : 'POST',
			url : "/R/pub/opencpu.demo/rpubhtml/json",
			data : {
				base64html : '"' + reportbase + '"'
			},		
			success : function(data, textStatus) {
				var output = jQuery.parseJSON(data);
				window.location.href = output.continueUrl;
			},
			error : function(xhr, textStatus, errorThrown) {
				alert("Oh no! An errror:\n\n" + xhr.responseText);
			},
			complete : function(){
				$('#publish').attr('class', 'btn');
			}	
		});
	}
	
	var postknit = function() {
		// read the current text
		var knittext = $("#knittext").val();
		// we encode the knit snippet in base64 in case of weird characters.
		var knitbase = btoa(knittext)
	
		$("#result").html("");
		// call OpenCPU:
		$.ajax({
			type : 'POST',
			url : "/R/pub/opencpu.demo/knithtml/file",
			data : {
				base64text : '"' + knitbase + '"'
			},
			success : function(data, textStatus) {
				LASTREPORT = data;
				$('#publish').attr("class", "btn")
				$("#result").html(data);
				$('pre code').each(function(i, e) {
					hljs.highlightBlock(e);
				});
				MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
			},
			error : function(xhr, textStatus, errorThrown) {
				alert("Oh no! An errror:\n\n" + xhr.responseText);
			},
			complete : function(){
				$('#knit').attr('class', 'btn');
			}
		});
	}
	
	$.ajax({
		url : "example.txt",
		success : function(data){
			$('#knittext').val(data);
		}
	});
	
	$.ajax({
		url : "headers.txt",
		success : function(data){
			HEADERS = data;
		}
	});
	
	$('#knit').click(function() {
		$(this).attr('class', 'btn disabled');
		$('#publish').attr("class", "btn disabled")
		postknit();
	});
	
	$('#publish').click(function() {
		$(this).attr('class', 'btn disabled');
		uploadhtml();
	});	
});	
