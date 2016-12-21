# Developer

Very raw map builder to place city and name on the pandemic map based on Snap svg.

1. Double clic any where on the map to place a new city; the new city appears on the top left corner (close to london)
2. Drag the city at the right place. Any time you drag any-thing the entire dump of the city data is written in the console, you can then copy paste its content to update the current data within `0.html`
3. Adjust the city name using the console: `CS.selectAll("#city48").attr({"name":"san-francisco"})` adjust with the last id generated (visible in the console since you probably alredy dragged the city at the right place, it is the last one in the dump)
4. Grab the content and replace it in the file.
5. Reload the page, the city should appear on the top of the city circle, drag the name around and again copy/paste the dumped content to update the file...
