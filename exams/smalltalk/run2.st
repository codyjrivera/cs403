FileStream fileIn: 'DynamicStack.st'.
ds := DynamicStack new init.
ds display.

FileStream fileIn: 'problem2.st'.
1 to: 10 do: [:j | ds push: j*j. ds display].
[ds isEmpty] whileFalse: [Transcript display: ds pop; tab. ds display].
1 to: 10 do: [:j | ds push: j*j. ds display].
1 to: 6  do: [:j | Transcript display: ds pop; tab. ds display].
1 to: 5  do: [:j | ds push: j*j*j. ds display].
