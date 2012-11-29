
$(document).ready(function() {
   $("div#content").append("<div id='webcam'></div>");
   $('div#content div canvas').parent().hide();
   $("#webcam").webcam({
      width: 320,
      height: 240,
      mode: "callback",
      swffile: "/jscam.swf",
      onTick: function() {},
      onSave: function() {},
      onCapture: function() {},
      debug: function(type, string) {
         if (type === "notify" && string === "Camera started") {
            $('#webcam').hide();
            $('div#content div canvas').parent().show();
         }
      },
      onLoad: function() {}
   });
});
