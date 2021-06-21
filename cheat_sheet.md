---
name: "Cheat Sheet"  
author: "Nick Riches"  
output:  
  html_document  
---



<table class="table table-striped" style="font-size: 18px; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Linguistic feature </th>
   <th style="text-align:left;"> Universal Search String </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Complex Sentences (defined as a sentence with more than one FINITE clause) </td>
   <td style="text-align:left;"> has2clauses, has3clauses, has4clauses, has5clauses, hasMultipleClauses, isComplex (i.e. has more than one clause) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Special types of sentence </td>
   <td style="text-align:left;"> hasPassive, hasRelativePronoun, hasRelativeClause </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mood </td>
   <td style="text-align:left;"> isDeclarative, isInterrogative, isQuestion, isImperative, isExclamative </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Expansions (this refers to whether a Noun Phrase or Verb Complex (which some refer to as "Verb Phrase") contains more than one element) </td>
   <td style="text-align:left;"> hasNPexpansion, hasVCexpansion, hasVPexpansion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Searching for particular word classes </td>
   <td style="text-align:left;"> hasAux, hasPrep, hasSConj, etc. (see "Colours" tab for details of the labels which are used for particular word classes) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sentence with negative particles (not/n't) </td>
   <td style="text-align:left;"> hasNeg </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Comments and tags (NB contents of tags can be searched) </td>
   <td style="text-align:left;"> hasComment, hasTag, hasTagCONTENTS_OF_TAG </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Searching by turn length </td>
   <td style="text-align:left;"> turn1 (= all turns of length 1), turn2, turn3, turn4, turn5, turn5plus </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Searching by verb form </td>
   <td style="text-align:left;"> hasPresTense/hasPresentTense, hasPastTense, hasPresParticiple/hasPresentParticiple, hasPastParticiple, hasInfinitive </td>
  </tr>
</tbody>
</table>
NB strings are NOT case-sensitive. Capitals have been added above to make the search terms easier to read, but you do not have to use capitals when conducting searches.
