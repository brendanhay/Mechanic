$(document).ready(function() {

    $('#armstrong').websocketchat({
	url: 'ws://czpdc23-98y2z3j:1234/armstrong',
	formatter: new ArmstrongClass()
    });

    $('#bert').websocketchat({
	url: 'ws://czpdc23-98y2z3j:1234/bert',
	formatter: {
	    encode: function(data) { return BERT.encode(data); },
	    decode: function(data) { return BERT.decode(data); }
	}
    });

    $('#ubf').websocketchat({
	url: 'ws://czpdc23-98y2z3j:1234/ubf',
	formatter: new ArmstrongClass()
    });


    $('a[href*=#]').click(function() {
	if (location.pathname.replace(/^\//,'') == this.pathname.replace(/^\//,'') 
            && location.hostname == this.hostname) {
            var $target = $(this.hash);
            $target = $target.length && $target || $('[name=' + this.hash.slice(1) +']');
            
            if ($target.length) {
                var targetOffset = $target.offset().top;
                
                $('html,body').animate({scrollTop: targetOffset}, 250);
                    
                return false;
            }
        }
    });

});

(function($) {

    $.fn.websocketchat = function(options){
	if (!options.url)
	    return;

	var ws;
	var fun = function() { };
        var defaults = { 
	    formatter: null,
	    controls: {
		connect: 'button.connect',
		disconnect: 'button.disconnect',
		input: 'input.input', 
		output: 'div.output',
		send: 'button.send'
	    },
	    css: {
		client: 'client',
		server: 'server'
	    }
	};

        var o = $.extend(defaults, options);

	if (o.formatter === null)
	    return;

	function toggleEnabled() {
	    var max = arguments.length;
	    
	    for (var i = 0; i < max; i++) {
    		var button = $(arguments[i]);
		
		if (button.is(':input')) {
    		    button.is(':disabled') 
    			? button.removeAttr('disabled')
     			: button.attr('disabled', 'disabled');
		}
	    }
	};
  	
	this.each(function() {
	    var $this = $(this);
	    var $connect = $(o.controls.connect, $this);
	    var $disconnect = $(o.controls.disconnect, $this);
	    var $send = $(o.controls.send, $this)
	    var $output = $(o.controls.output, $this);
	    var $input = $(o.controls.input, $this);

	    console.log($input);

	    var write = function(str, css, name) {
	    
		var msg = '<p>' + name + ': <span class="' + css + '">' + str + '</span></p>';
	    	$output.append(msg);
		
	    	var bottom = $output.attr('scrollHeight');
	    	$output.scrollTop(bottom);
	    };
	    
	    var client = function(str) {
	    	write(str, o.css.client, 'Client');
	    };
	    
	    var server = function(str) {
	    	write(str, o.css.server, 'Server');
	    };
	        
	    var create = function(url) {
    		var socket = new WebSocket(url);

		socket.onopen = function() {
    		    client('Socket connected');
		    
    		    toggleEnabled($connect, $send, $disconnect);
		    
		    //socket.send(o.formatter.encode("Your browser time is - " + new Date()));
    		};

    		socket.onmessage = function(event) {
		    var value = o.formatter.decode(event.data);
    		    server(value);
    		};
		
    		socket.onclose = function() {
    		    client('Socket closed');
		    
    		    toggleEnabled($connect, $send, $disconnect);
    		};
		
    		return socket;
	    };

	    client('Loaded ' + $this.attr('id') + '.js');

	    if ('WebSocket' in window) {
    		client('WebSockets supported'); 
		
    		var ws;
    		$connect.click(function() {
    		    ws = create(o.url);	    
    		});

		$input.keypress(function () {
		    var enterKey = 13;
		    if ((event.which && event.which == 13) 
			|| (event.keyCode && event.keyCode == 27)) {
			$send.click();
		    }
		});

    		$send.click(function() {
    		    var value = $input.val();
    		    if (value) {
    			ws.send(o.formatter.encode(value));
    			$input.val('');     
    		    }
		    
    		    $input.focus();
		    
    		    return true;            
    		});
		
    		$disconnect.click(function() {
    		    ws.close();
    		});
	    } else {
    		client("Your browser does not support websockets");
	    };
	});
    } 
})($); 
