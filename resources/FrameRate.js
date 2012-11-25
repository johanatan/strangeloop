
var FrameRate = function() {
   var fps = 40;
   var msPerFrame = 1000 / fps;

   // Set the desired rate of frames per second.
   // Default is 40 if no fps event is triggered.
   document.addEventListener('fps', function(e) {
      console.log(e.value);
      fps = e.value;
      msPerFrame = 1000 / fps;
   });

   var date = new Date();
   var startTime = date.getTime();
   var frameStart = startTime;
   var frameEnd   = startTime;
   var timeoutID = 0;

   var signalEvent = function(eventName, value) {
      var e = document.createEvent('Event');
      e.initEvent(eventName, true, true);
      e.value = value;
      document.dispatchEvent(e);
   }

   // Trigger time events to try to maintain the desired FPS.
   document.addEventListener('finished', function(evt) {
      clearTimeout(timeoutID);
      date = new Date();
      frameEnd = date.getTime();
      var diff = frameEnd - frameStart;
      var waitTime = Math.max(msPerFrame - diff, 3);
      timeoutID = setTimeout(function() {
         date = new Date();
         frameStart = date.getTime();
         signalEvent('trigger', (frameStart - startTime) / 1000);
          }, waitTime);
   });
}();
