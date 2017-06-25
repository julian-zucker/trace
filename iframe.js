var nodes = document.evaluate("//span/a", 
        document.getElementById("contentFrame").contentWindow.document, 
        null, XPathResult.ANY_TYPE, null);

var viewAllButton = nodes.iterateNext();

viewAllButton.click();