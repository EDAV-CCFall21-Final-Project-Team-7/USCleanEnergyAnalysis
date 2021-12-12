      var width = document.getElementById('vis')
          .clientWidth;
      var height = document.getElementById('vis')
          .clientHeight;
      
      var margin = {
          top: 10,
          bottom: 70,
          left: 70,
          right: 20
      }
      
      var svg = d3.select('#vis')
          .append('svg')
          .attr('width', width)
          .attr('height', height)
          .append('g')
          .attr('transform', 'translate(' + margin.left + ',' + margin.right + ')');
      
      width = width - margin.left - margin.right;
      height = height - margin.top - margin.bottom;
      
      var data = {};
      
      var y_scale = d3.scaleBand()
          .rangeRound([0, height])
          .padding(0.1);
      
      var x_scale = d3.scaleLinear()
          .range([0, width]);
      
      var colour_scale = d3.scaleQuantile()
          .range(["#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"]);
      
      
      var y_axis = d3.axisLeft(y_scale);
      var x_axis = d3.axisBottom(x_scale);
      
      svg.append('g')
          .attr('class', 'x axis')
          .attr('transform', 'translate(0,' + height + ')');
      
      svg.append('g')
          .attr('class', 'y axis');
      
      function draw(year) {
        
          d3.csv('https://raw.githubusercontent.com/EDAV-CCFall21-Final-Project-Team-7/USCleanEnergyAnalysis/main/draft%20code/d'+year).then( function(csv_data){
            
            var t = d3.transition()
                .duration(2000);
                
            var months = csv_data.map(function(d) {
                return d.month;
            });
            y_scale.domain(months);
            
            var max_value = d3.max(csv_data, function(d) {
                return +d.value;
            });
        
            x_scale.domain([0, 100]);
            colour_scale.domain([0, max_value]);
        
            var bars = svg.selectAll('.bar')
                .data(csv_data)
        
            bars
                .exit()
                .remove();
                
            var new_bars = bars
                .enter()
                .append('rect')
                .attr('class', 'bar')
                .attr('y', function(d) {
                    return y_scale(d.month);
                })
                .attr('height', y_scale.bandwidth())
                .attr('x', x_scale(0))
                .attr('width', d => x_scale(+d.value))
                

        //move bars with size, then change axis name
            new_bars.merge(bars)
                .transition(t)
                .attr('x', function(d) {
                    return y_scale(+d.value);
                })
                .attr('width', function(d) {
                    return x_scale(+d.value)
                })
                .attr('fill', function(d) {
                    return colour_scale(+d.value);
                })
        
            svg.select('.x.axis')
                .call(x_axis);
        
            svg.select('.y.axis')
                .transition(t)
                .call(y_axis);
                
            svg.append("text")
              .attr("class", "x label")
              .attr("text-anchor", "end")
              .attr("x", width/1.6)
              .attr("y", height+30)
              .text("% of energy that is renewable (in each state)");
            
            svg.append("text")
              .attr("class", "y label")
              .attr("text-anchor", "end")
              .attr("x",-300)
              .attr("y", -50)
              .attr("dy", ".75em")
              .attr("transform", "rotate(-90)")
              .text("State");  
              
            
            svg.append("text")
              .attr("class", "title")
              .attr("text-anchor", "end")
              .attr("x", width/1.45)
              .attr("y", height-730)
              .text("Bar chart showing % of energy that is renewable (in each state)"); 
              
            
          });
      
      }
      
      draw(2021);
      
      var slider = d3.select('#year');
      slider.on('change', function() {
          draw(this.value);
      });
