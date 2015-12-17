# StatholScript

StatholScript (from OE staðol) is a basic scripting language made as part of a Directed
Study at Hanover College. It was made with functional programming techniques in mind.

## Testing

Clone the repository and navigate to the source folder, then run the commands

     ./make
     ./lang < ../doc/validTests
     ./land < ../doc/invalidTests

Both tests will enter each expression into the interpreter one at a time. Everything in the validTests file should return a value of true, while everything in the invalidTests file should return an error (that is caught by the interpreter).
