# reactif-lib
Prototype implementation of the OM reactive model for OM 6.7/6.8


Reactive programming in OM is an intermediate model for visual programming inheriting both the off-line, demand-driven, computation paradigm of OpenMusic, and the reactive features of interactive/real-time systems. The objective is to integrate and control the interactions between the calculi denoted by CAC programs and their external context. In this model visual programming components are capable of "listening" to the environment or to other components, updating contents or triggering calculations accordingly, and propagating changes in a determined way in the program. This "dataflow-oriented" approach implies new semantic definitions, implementation and corresponding user interactions. It is based on the existing OM semantics and conservative with regard to this semantics, so that existing programs created in the environment remain valid in the reactive framework.

The OM reactive model is implemented as a native feature in OM >= 6.9.

See also: http://repmus.ircam.fr/openmusic/reactive
