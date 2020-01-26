# Notes on Codec for Serialization and Deserialization

https://xrpl.org/serialization.html


# Objects and PathSets
- Objects require filtering and sorting of fields
= PathSets require sorting the encoding of the atoms in a PathStep

# Mappings

The full bi-directional path is left <-> right but sometimes I will skip the model


JSON || MODEL || ENCODER || DECODER | MODEL | JSON


VL Encoding location still in question. At the field level we know, and when we implement at the nested level we know.
The question is aside from Account Address are there any times these do not match ?
(Account Address always VL encoded at top level)
