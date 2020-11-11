FileStream fileIn: 'DynamicStack.st'.
ds := DynamicStack new init.
ds display.

FileStream fileIn: 'problem2.st'.
1 to: 10 do: [:j | ds push: j*j. ds display].
[ds isEmpty] whileFalse: [Transcript display: ds pop; tab. ds display].
