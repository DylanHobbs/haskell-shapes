# shapesSVG

Assignment 1 - Combining the Shape eDSL with two web eDSL languages we saw (Scotty and Blaze) to produce a web application capable of delivering SVG objects.

### Usage
 - stack init
 - stack build
 - stack exec shapesSVG
 - Navigate to "http://localhost:3000/shapes"

 A drawing is of the form [(styles, transforms, shapes)]
 Entering this into the text box will output a SVG or just press go for an example.

### Shapes Supported:
  Rectangle     (Double::height Double::width)
  Circle        (Double::radius
  
### Sytles 
  All colours are RGB formed with 3 doubles in the range of 0.0-0.1
  NoStyle       (No style)
  FillColour    (Double::red Double::green Double::blue)
  StrokeWidth   (Double::thickness)
  StrokeColour  (Double::red Double::green Double::blue)
  
### Transforms
  Identity      (No transform)
  Translate     (Double::x Double::y)
  Scale         (Double::x Double::y)
  Rotate        (Int   ::degree)
  
### Combinations
  Inline combiners can be used to combine both Transforms and Styles
  For styles use the operator: ":<++>"
  For transforms use         : ":<+>"
  Brackets will still be needed for precedence for the read instance to take. eg:
    - FillColour 0 0 0 :<++> (StrokeWidth 1 :<++> StrokeColour 0.5 0.5 0.5) 
