
$(document).ready(function() {
   var camWidth = 320;
   var camHeight = 240;
   $("div#content").append("<div id='webcam'></div>");
   $('div#content div canvas').parent().hide();
   var count = 0;
   var canvas = document.createElement("canvas");
   canvas.setAttribute('width', camWidth);
   canvas.setAttribute('height', camHeight);
   var ctx = canvas.getContext("2d");
   var img = ctx.getImageData(0, 0, camWidth, camHeight);
   var pos = 0;
   $("#webcam").webcam({
      width: camWidth,
      height: camHeight,
      mode: "stream",
      swffile: "/jscam.swf",
      onTick: function() {},
      onSave: function(data) {
         var col = data.split(";");
         for (var i = 0; i < camWidth; i++) {
            var tmp = parseInt(col[i]);
            img.data[pos + 0] = (tmp >> 16) & 0xff;
            img.data[pos + 1] = (tmp >> 8) & 0xff;
            img.data[pos + 2] = tmp & 0xff;
            img.data[pos + 3] = 0xff;
            pos += 4;
         }
         if (pos >= 4 * camWidth * camHeight) {
            ctx.putImageData(img, 0, 0);
            signalEvent('webcamFrame', canvas.toDataURL());
            pos = 0;
         }
      },
      onCapture: function() {},
      debug: function(type, string) {
         if (type === "notify" && string === "Camera started") {
            $('#webcam').offset({ top: 0, left: -1 - camWidth });
            $('div#content div canvas').parent().show();
            webcam.capture();
         }
      },
      onLoad: function() {}
   });
});
