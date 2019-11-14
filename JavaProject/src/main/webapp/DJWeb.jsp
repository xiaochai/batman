<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
    <head>
        <title>DJWeb</title>
    </head>
    <body>
        <h2>Current BPM: ${bpm}</h2>
        <form method="get">
          <button type="submit" name="value" value="1" >上升</button>
          <button type="submit" name="value" value="-1">下降</button>
        </form>
    </body>
</html>