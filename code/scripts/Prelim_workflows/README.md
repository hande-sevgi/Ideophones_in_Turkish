# Computational annotation workflow

The open-ended responses vary considerably in their internal structure. Some participants supplied a single word, others supplied a multiword phrase, and some produced responses containing multiple potentially relevant elements or a complete clause. The analysis therefore does not assume that every response corresponds to one word, one phrase, or one part-of-speech category.

The annotation workflow distinguishes three analytical levels:

1. **Response level:** the participant’s complete original answer;
2. **Target-element level:** one or more linguistically relevant units identified within the response;
3. **Token level:** the individual words generated through automatic tokenization.

Automatic Turkish annotation is performed using Stanza. For each token, the workflow retains the written form, predicted lemma, and predicted universal part-of-speech tag. These predictions are treated as preliminary computational annotations rather than definitive linguistic analyses.

The original responses and Stanza predictions are preserved unchanged. Potential inconsistencies are recorded in separate manual-annotation fields. This makes it possible to distinguish the model’s output from subsequent linguistic decisions.

Particular attention is given to multiword constructions. For example, a reduplicated expression such as `hüngür hüngür` contains two tokens but constitutes a single construction. Similarly, a response may contain a discourse marker together with a temporal, manner, locative, or participant-denoting expression. Responses containing multiple target elements are segmented and assigned separate target identifiers rather than being forced into a single category.

The workflow follows these stages:

1. preserve and normalize the original response text;
2. assign a permanent response identifier;
3. tokenize the response and obtain Stanza lemma and POS predictions;
4. detect candidate multiword constructions and responses containing multiple elements;
5. manually review segmentation and questionable automatic annotations;
6. record corrections without overwriting the Stanza output;
7. construct final token- and target-level annotations; and
8. validate the completed annotations against the accompanying codebook.

The codebook is developed iteratively. Recurring constructions, ambiguous cases, and systematic model errors are documented as explicit annotation decisions, together with definitions, examples, and the reasoning behind each decision.

As expected, stanza is working poorly (at least for the current purposes). On the ten-response pilot sample, the Turkish spaCy model produced more linguistically useful tokenization, POS, morphological, and dependency annotations than Stanza for the present research purpose.
