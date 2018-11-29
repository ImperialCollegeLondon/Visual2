/*
Visual2 @ Imperial College London
Project: A user - friendly ARM emulator in F# and Web Technologies(Github Electron & Fable Compliler)
Module: JS/Monaco-init
Description: Javascript code to run Monaco editor and define code highlighting regexes
*/


// Save Monaco's amd require and restore Node's require
var amdRequire = global.require;
global.require = nodeRequire;

//
// get tippy.js node module for awesome tooltips
// global.tippy = require('tippy.js');
// NB this does not package properly:
// SOLUTION: use tippy-all-min.js
// add to app/js directory
// add <script src="js/tippy.all.min.js"></script> to index.html
// (optional, but best practice) to allow update via yarn, 
// copy this file from ./node_modules/tippy.js to app/js via webpack.config.js CopyWebpackPlugin
//


// todo: why does this work when tippy.js does not?
var path = require('path');

function uriFromPath(_path) {
  var pathName = path.resolve(_path).replace(/\\/g, '/');
  if (pathName.length > 0 && pathName.charAt(0) !== '/') {
    pathName = '/' + pathName;
  }
  return encodeURI('file://' + pathName);
}
amdRequire.config({
  //baseUrl: uriFromPath(path.join(__dirname, '../node_modules/monaco-editor/min'))
  baseUrl: uriFromPath(path.join(__dirname, 'js'))

});
// workaround monaco-css not understanding the environment
self.module = undefined;
// workaround monaco-typescript not understanding the environment
self.process.browser = true;
amdRequire(['vs/editor/editor.main'], function () {

  monaco.languages.register({
    id: 'arm'
  });
  monaco.languages.setMonarchTokensProvider('arm', {
    // Set defaultToken to invalid to see what you do not tokenize yet
    defaultToken: 'invalid',

    ignoreCase: true,

    brackets: [
      ['{', '}', 'delimiter.curly'],
      ['[', ']', 'delimiter.square'],
      ['(', ')', 'delimiter.parenthesis'],
      ['<', '>', 'delimiter.angle']
    ],

    operators: [
      '+', '-', '*',',', '='
    ],

    keywords: [
      "ADC","ADCCS","ADCEQ","ADCGE","ADCGT","ADCHI","ADCHS","ADCLE","ADCLO","ADCLS","ADCLT","ADCMI",
      "ADCNE","ADCCC","ADCPL","ADCS","ADCSCS","ADCSEQ","ADCSGE","ADCSGT","ADCSHI","ADCSHS","ADCSLE",
      "ADCSLO","ADCSLS","ADCSLT","ADCSMI","ADCSNE","ADCSCC","ADCSPL","ADCSVC","ADCSVS","ADCVC","ADCVS","ADD",
      "ADDCS","ADDEQ","ADDGE","ADDGT","ADDHI","ADDHS","ADDLE","ADDLO","ADDLS","ADDLT","ADDMI","ADDNE","ADDCC",
      "ADDPL","ADDS","ADDSCS","ADDSEQ","ADDSGE","ADDSGT","ADDSHI","ADDSHS","ADDSLE","ADDSLO","ADDSLS","ADDSLT",
      "ADDSMI","ADDSNE","ADDSCC","ADDSPL","ADDSVC","ADDSVS","ADDVC","ADDVS","AND","ANDCS","ANDEQ","ANDGE","ANDGT",
      "ANDHI","ANDHS","ANDLE","ANDLO","ANDLS","ANDLT","ANDMI","ANDNE","ANDCC","ANDPL","ANDS","ANDSCS","ANDSEQ",
      "ANDSGE","ANDSGT","ANDSHI","ANDSHS","ANDSLE","ANDSLO","ANDSLS","ANDSLT","ANDSMI","ANDSNE","ANDSCC","ANDSPL",
      "ANDSVC","ANDSVS","ANDVC","ANDVS","ASR","ASRCS","ASREQ","ASRGE","ASRGT","ASRHI","ASRHS","ASRLE","ASRLO",
      "ASRLS","ASRLT","ASRMI","ASRNE","ASRCC","ASRPL","ASRS","ASRSCS","ASRSEQ","ASRSGE","ASRSGT","ASRSHI","ASRSHS",
      "ASRSLE","ASRSLO","ASRSLS","ASRSLT","ASRSMI","ASRSNE","ASRSCC","ASRSPL","ASRSVC","ASRSVS","ASRVC","ASRVS",
      "BIC","BICCS","BICEQ","BICGE","BICGT","BICHI","BICHS","BICLE","BICLO","BICLS","BICLT","BICMI","BICNE","BICCC",
      "BICPL","BICS","BICSCS","BICSEQ","BICSGE","BICSGT","BICSHI","BICSHS","BICSLE","BICSLO","BICSLS","BICSLT",
      "BICSMI","BICSNE","BICSCC","BICSPL","BICSVC","BICSVS","BICVC","BICVS","CMN","CMNCS","CMNEQ","CMNGE","CMNGT",
      "CMNHI","CMNHS","CMNLE","CMNLO","CMNLS","CMNLT","CMNMI","CMNNE","CMNCC","CMNPL","CMNS","CMNSCS","CMNSEQ",
      "CMNSGE","CMNSGT","CMNSHI","CMNSHS","CMNSLE","CMNSLO","CMNSLS","CMNSLT","CMNSMI","CMNSNE","CMNSCC","CMNSPL",
      "CMNSVC","CMNSVS","CMNVC","CMNVS","CMP","CMPCS","CMPEQ","CMPGE","CMPGT","CMPHI","CMPHS","CMPLE","CMPLO",
      "CMPLS","CMPLT","CMPMI","CMPNE","CMPCC","CMPPL","CMPS","CMPSCS","CMPSEQ","CMPSGE","CMPSGT","CMPSHI","CMPSHS",
      "CMPSLE","CMPSLO","CMPSLS","CMPSLT","CMPSMI","CMPSNE","CMPSCC","CMPSPL","CMPSVC","CMPSVS","CMPVC","CMPVS",
      "EOR","EORCS","EOREQ","EORGE","EORGT","EORHI","EORHS","EORLE","EORLO","EORLS","EORLT","EORMI","EORNE","EORCC",
      "EORPL","EORS","EORSCS","EORSEQ","EORSGE","EORSGT","EORSHI","EORSHS","EORSLE","EORSLO","EORSLS","EORSLT",
      "EORSMI","EORSNE","EORSCC","EORSPL","EORSVC","EORSVS","EORVC","EORVS","LSL","LSLCS","LSLEQ","LSLGE","LSLGT",
      "LSLHI","LSLHS","LSLLE","LSLLO","LSLLS","LSLLT","LSLMI","LSLNE","LSLCC","LSLPL","LSLS","LSLSCS","LSLSEQ",
      "LSLSGE","LSLSGT","LSLSHI","LSLSHS","LSLSLE","LSLSLO","LSLSLS","LSLSLT","LSLSMI","LSLSNE","LSLSCC","LSLSPL",
      "LSLSVC","LSLSVS","LSLVC","LSLVS","LSR","LSRCS","LSREQ","LSRGE","LSRGT","LSRHI","LSRHS","LSRLE","LSRLO",
      "LSRLS","LSRLT","LSRMI","LSRNE","LSRCC","LSRPL","LSRS","LSRSCS","LSRSEQ","LSRSGE","LSRSGT","LSRSHI","LSRSHS",
      "LSRSLE","LSRSLO","LSRSLS","LSRSLT","LSRSMI","LSRSNE","LSRSCC","LSRSPL","LSRSVC","LSRSVS","LSRVC","LSRVS",
      "MOV","MOVCS","MOVEQ","MOVGE","MOVGT","MOVHI","MOVHS","MOVLE","MOVLO","MOVLS","MOVLT","MOVMI","MOVNE","MOVCC",
      "MOVPL","MOVS","MOVSCS","MOVSEQ","MOVSGE","MOVSGT","MOVSHI","MOVSHS","MOVSLE","MOVSLO","MOVSLS","MOVSLT",
      "MOVSMI","MOVSNE","MOVSCC","MOVSPL","MOVSVC","MOVSVS","MOVVC","MOVVS","MVN","MVNCS","MVNEQ","MVNGE",
      "MVNGT","MVNHI","MVNHS","MVNLE","MVNLO","MVNLS","MVNLT","MVNMI","MVNNE","MVNCC","MVNPL","MVNS","MVNSCS",
      "MVNSEQ","MVNSGE","MVNSGT","MVNSHI","MVNSHS","MVNSLE","MVNSLO","MVNSLS","MVNSLT","MVNSMI","MVNSNE",
      "MVNSCC","MVNSPL","MVNSVC","MVNSVS","MVNVC","MVNVS","ORR","ORRCS","ORREQ","ORRGE","ORRGT","ORRHI",
      "ORRHS","ORRLE","ORRLO","ORRLS","ORRLT","ORRMI","ORRNE","ORRCC","ORRPL","ORRS","ORRSCS","ORRSEQ","ORRSGE",
      "ORRSGT","ORRSHI","ORRSHS","ORRSLE","ORRSLO","ORRSLS","ORRSLT","ORRSMI","ORRSNE","ORRSCC","ORRSPL","ORRSVC",
      "ORRSVS","ORRVC","ORRVS","ROR","RORCS","ROREQ","RORGE","RORGT","RORHI","RORHS","RORLE","RORLO","RORLS","RORLT",
      "RORMI","RORNE","RORCC","RORPL","RORS","RORSCS","RORSEQ","RORSGE","RORSGT","RORSHI","RORSHS","RORSLE","RORSLO",
      "RORSLS","RORSLT","RORSMI","RORSNE","RORSCC","RORSPL","RORSVC","RORSVS","RORVC","RORVS","RRX","RRXCS","RRXEQ",
      "RRXGE","RRXGT","RRXHI","RRXHS","RRXLE","RRXLO","RRXLS","RRXLT","RRXMI","RRXNE","RRXCC","RRXPL","RRXS","RRXSCS",
      "RRXSEQ","RRXSGE","RRXSGT","RRXSHI","RRXSHS","RRXSLE","RRXSLO","RRXSLS","RRXSLT","RRXSMI","RRXSNE","RRXSCC",
      "RRXSPL","RRXSVC","RRXSVS","RRXVC","RRXVS","RSB","RSBCS","RSBEQ","RSBGE","RSBGT","RSBHI","RSBHS","RSBLE",
      "RSBLO","RSBLS","RSBLT","RSBMI","RSBNE","RSBCC","RSBPL","RSBS","RSBSCS","RSBSEQ","RSBSGE","RSBSGT","RSBSHI",
      "RSBSHS","RSBSLE","RSBSLO","RSBSLS","RSBSLT","RSBSMI","RSBSNE","RSBSCC","RSBSPL","RSBSVC","RSBSVS","RSBVC",
      "RSBVS","RSC","RSCCS","RSCEQ","RSCGE","RSCGT","RSCHI","RSCHS","RSCLE","RSCLO","RSCLS","RSCLT","RSCMI","RSCNE",
      "RSCCC","RSCPL","RSCS","RSCSCS","RSCSEQ","RSCSGE","RSCSGT","RSCSHI","RSCSHS","RSCSLE","RSCSLO","RSCSLS","RSCSLT",
      "RSCSMI","RSCSNE","RSCSCC","RSCSPL","RSCSVC","RSCSVS","RSCVC","RSCVS","SBC","SBCCS","SBCEQ","SBCGE","SBCGT",
      "SBCHI","SBCHS","SBCLE","SBCLO","SBCLS","SBCLT","SBCMI","SBCNE","SBCCC","SBCPL","SBCS","SBCSCS","SBCSEQ",
      "SBCSGE","SBCSGT","SBCSHI","SBCSHS","SBCSLE","SBCSLO","SBCSLS","SBCSLT","SBCSMI","SBCSNE","SBCSCC","SBCSPL",
      "SBCSVC","SBCSVS","SBCVC","SBCVS","SUB","SUBCS","SUBEQ","SUBGE","SUBGT","SUBHI","SUBHS","SUBLE","SUBLO",
      "SUBLS","SUBLT","SUBMI","SUBNE","SUBCC","SUBPL","SUBS","SUBSCS","SUBSEQ","SUBSGE","SUBSGT","SUBSHI","SUBSHS",
      "SUBSLE","SUBSLO","SUBSLS","SUBSLT","SUBSMI","SUBSNE","SUBSCC","SUBSPL","SUBSVC","SUBSVS","SUBVC","SUBVS",
      "TEQ","TEQCS","TEQEQ","TEQGE","TEQGT","TEQHI","TEQHS","TEQLE","TEQLO","TEQLS","TEQLT","TEQMI","TEQNE","TEQCC",
      "TEQPL","TEQS","TEQSCS","TEQSEQ","TEQSGE","TEQSGT","TEQSHI","TEQSHS","TEQSLE","TEQSLO","TEQSLS","TEQSLT",
      "TEQSMI","TEQSNE","TEQSCC","TEQSPL","TEQSVC","TEQSVS","TEQVC","TEQVS","TST","TSTCS","TSTEQ","TSTGE","TSTGT",
      "TSTHI","TSTHS","TSTLE","TSTLO","TSTLS","TSTLT","TSTMI","TSTNE","TSTCC","TSTPL","TSTS","TSTSCS","TSTSEQ",
      "TSTSGE","TSTSGT","TSTSHI","TSTSHS","TSTSLE","TSTSLO","TSTSLS","TSTSLT","TSTSMI","TSTSNE","TSTSCC","TSTSPL",
      "TSTSVC","TSTSVS","TSTVC","TSTVS","B","BCS","BEQ","BGE","BGT","BHI","BHS","BL","BLCS","BLE","BLEQ","BLGE",
      "BLGT","BLHI","BLHS","BLLE","BLLO","BLLS","BLLT","BLMI","BLNE","BLCC","BLO","BLPL","BLS","BLT","BLVC","BLVS",
      "BMI","BNE","BCC","BPL","BVC","BVS","END","ENDCS","ENDEQ","ENDGE","ENDGT","ENDHI","ENDHS","ENDLE","ENDLO",
      "ENDLS","ENDLT","ENDMI","ENDNE","ENDCC","ENDPL","ENDVC","ENDVS","LDM","LDMCS","LDMB","LDMBCS","LDMBEQ","LDMBGE",
      "LDMBGT","LDMBHI","LDMBHS","LDMBLE","LDMBLO","LDMBLS","LDMBLT","LDMBMI","LDMBNE","LDMBCC","LDMBPL","LDMBVC",
      "LDMBVS","LDMDA","LDMDACS","LDMDAEQ","LDMDAGE","LDMDAGT","LDMDAHI","LDMDAHS","LDMDALE","LDMDALO","LDMDALS",
      "LDMDALT","LDMDAMI","LDMDANE","LDMDACC","LDMDAPL","LDMDAVC","LDMDAVS","LDMDB","LDMDBCS","LDMDBEQ","LDMDBGE",
      "LDMDBGT","LDMDBHI","LDMDBHS","LDMDBLE","LDMDBLO","LDMDBLS","LDMDBLT","LDMDBMI","LDMDBNE","LDMDBCC","LDMDBPL",
      "LDMDBVC","LDMDBVS","LDMEA","LDMEACS","LDMEAEQ","LDMEAGE","LDMEAGT","LDMEAHI","LDMEAHS","LDMEALE","LDMEALO",
      "LDMEALS","LDMEALT","LDMEAMI","LDMEANE","LDMEACC","LDMEAPL","LDMEAVC","LDMEAVS","LDMED","LDMEDCS","LDMEDEQ",
      "LDMEDGE","LDMEDGT","LDMEDHI","LDMEDHS","LDMEDLE","LDMEDLO","LDMEDLS","LDMEDLT","LDMEDMI","LDMEDNE","LDMEDCC",
      "LDMEDPL","LDMEDVC","LDMEDVS","LDMEQ","LDMFA","LDMFACS","LDMFAEQ","LDMFAGE","LDMFAGT","LDMFAHI","LDMFAHS",
      "LDMFALE","LDMFALO","LDMFALS","LDMFALT","LDMFAMI","LDMFANE","LDMFACC","LDMFAPL","LDMFAVC","LDMFAVS","LDMFD","LDMFDCS",
      "LDMFDEQ","LDMFDGE","LDMFDGT","LDMFDHI","LDMFDHS","LDMFDLE","LDMFDLO","LDMFDLS","LDMFDLT","LDMFDMI","LDMFDNE",
      "LDMFDCC","LDMFDPL","LDMFDVC","LDMFDVS","LDMGE","LDMGT","LDMHI","LDMHS","LDMIA","LDMIACS","LDMIAEQ","LDMIAGE",
      "LDMIAGT","LDMIAHI","LDMIAHS","LDMIALE","LDMIALO","LDMIALS","LDMIALT","LDMIAMI","LDMIANE","LDMIACC","LDMIAPL",
      "LDMIAVC","LDMIAVS","LDMIB","LDMIBCS","LDMIBEQ","LDMIBGE","LDMIBGT","LDMIBHI","LDMIBHS","LDMIBLE","LDMIBLO",
      "LDMIBLS","LDMIBLT","LDMIBMI","LDMIBNE","LDMIBCC","LDMIBPL","LDMIBVC","LDMIBVS","LDMLE","LDMLO","LDMLS","LDMLT","LDMMI",
      "LDMNE","LDMCC","LDMPL","LDMVC","LDMVS","LDR","LDRCS","LDRB","LDRBCS","LDRBEQ","LDRBGE","LDRBGT","LDRBHI",
      "LDRBHS","LDRBLE","LDRBLO","LDRBLS","LDRBLT","LDRBMI","LDRBNE","LDRBCC","LDRBPL","LDRBVC","LDRBVS","LDRDA",
      "LDRDACS","LDRDAEQ","LDRDAGE","LDRDAGT","LDRDAHI","LDRDAHS","LDRDALE","LDRDALO","LDRDALS","LDRDALT","LDRDAMI",
      "LDRDANE","LDRDACC","LDRDAPL","LDRDAVC","LDRDAVS","LDRDB","LDRDBCS","LDRDBEQ","LDRDBGE","LDRDBGT","LDRDBHI",
      "LDRDBHS","LDRDBLE","LDRDBLO","LDRDBLS","LDRDBLT","LDRDBMI","LDRDBNE","LDRDBCC","LDRDBPL","LDRDBVC","LDRDBVS",
      "LDREA","LDREACS","LDREAEQ","LDREAGE","LDREAGT","LDREAHI","LDREAHS","LDREALE","LDREALO","LDREALS","LDREALT",
      "LDREAMI","LDREANE","LDREACC","LDREAPL","LDREAVC","LDREAVS","LDRED","LDREDCS","LDREDEQ","LDREDGE","LDREDGT","LDREDHI",
      "LDREDHS","LDREDLE","LDREDLO","LDREDLS","LDREDLT","LDREDMI","LDREDNE","LDREDCC","LDREDPL","LDREDVC","LDREDVS","LDREQ",
      "LDRFA","LDRFACS","LDRFAEQ","LDRFAGE","LDRFAGT","LDRFAHI","LDRFAHS","LDRFALE","LDRFALO","LDRFALS","LDRFALT","LDRFAMI",
      "LDRFANE","LDRFACC","LDRFAPL","LDRFAVC","LDRFAVS","LDRFD","LDRFDCS","LDRFDEQ","LDRFDGE","LDRFDGT","LDRFDHI","LDRFDHS",
      "LDRFDLE","LDRFDLO","LDRFDLS","LDRFDLT","LDRFDMI","LDRFDNE","LDRFDCC","LDRFDPL","LDRFDVC","LDRFDVS","LDRGE","LDRGT",
      "LDRHI","LDRHS","LDRIA","LDRIACS","LDRIAEQ","LDRIAGE","LDRIAGT","LDRIAHI","LDRIAHS","LDRIALE","LDRIALO","LDRIALS",
      "LDRIALT","LDRIAMI","LDRIANE","LDRIACC","LDRIAPL","LDRIAVC","LDRIAVS","LDRIB","LDRIBCS","LDRIBEQ","LDRIBGE","LDRIBGT",
      "LDRIBHI","LDRIBHS","LDRIBLE","LDRIBLO","LDRIBLS","LDRIBLT","LDRIBMI","LDRIBNE","LDRIBCC","LDRIBPL","LDRIBVC","LDRIBVS",
      "LDRLE","LDRLO","LDRLS","LDRLT","LDRMI","LDRNE","LDRCC","LDRPL","LDRVC","LDRVS","STM","STMCS","STMB","STMBCS","STMBEQ",
      "STMBGE","STMBGT","STMBHI","STMBHS","STMBLE","STMBLO","STMBLS","STMBLT","STMBMI","STMBNE","STMBCC","STMBPL","STMBVC",
      "STMBVS","STMDA","STMDACS","STMDAEQ","STMDAGE","STMDAGT","STMDAHI","STMDAHS","STMDALE","STMDALO","STMDALS","STMDALT",
      "STMDAMI","STMDANE","STMDACC","STMDAPL","STMDAVC","STMDAVS","STMDB","STMDBCS","STMDBEQ","STMDBGE","STMDBGT","STMDBHI",
      "STMDBHS","STMDBLE","STMDBLO","STMDBLS","STMDBLT","STMDBMI","STMDBNE","STMDBCC","STMDBPL","STMDBVC","STMDBVS","STMEA",
      "STMEACS","STMEAEQ","STMEAGE","STMEAGT","STMEAHI","STMEAHS","STMEALE","STMEALO","STMEALS","STMEALT","STMEAMI","STMEANE",
      "STMEACC","STMEAPL","STMEAVC","STMEAVS","STMED","STMEDCS","STMEDEQ","STMEDGE","STMEDGT","STMEDHI","STMEDHS","STMEDLE",
      "STMEDLO","STMEDLS","STMEDLT","STMEDMI","STMEDNE","STMEDCC","STMEDPL","STMEDVC","STMEDVS","STMEQ","STMFA","STMFACS",
      "STMFAEQ","STMFAGE","STMFAGT","STMFAHI","STMFAHS","STMFALE","STMFALO","STMFALS","STMFALT","STMFAMI","STMFANE","STMFACC",
      "STMFAPL","STMFAVC","STMFAVS","STMFD","STMFDCS","STMFDEQ","STMFDGE","STMFDGT","STMFDHI","STMFDHS","STMFDLE","STMFDLO",
      "STMFDLS","STMFDLT","STMFDMI","STMFDNE","STMFDCC","STMFDPL","STMFDVC","STMFDVS","STMGE","STMGT","STMHI","STMHS","STMIA",
      "STMIACS","STMIAEQ","STMIAGE","STMIAGT","STMIAHI","STMIAHS","STMIALE","STMIALO","STMIALS","STMIALT","STMIAMI","STMIANE",
      "STMIACC","STMIAPL","STMIAVC","STMIAVS","STMIB","STMIBCS","STMIBEQ","STMIBGE","STMIBGT","STMIBHI","STMIBHS","STMIBLE",
      "STMIBLO","STMIBLS","STMIBLT","STMIBMI","STMIBNE","STMIBCC","STMIBPL","STMIBVC","STMIBVS","STMLE","STMLO","STMLS","STMLT",
      "STMMI","STMNE","STMCC","STMPL","STMVC","STMVS","STR","STRCS","STRB","STRBCS","STRBEQ","STRBGE","STRBGT","STRBHI","STRBHS",
      "STRBLE","STRBLO","STRBLS","STRBLT","STRBMI","STRBNE","STRBCC","STRBPL","STRBVC","STRBVS","STRDA","STRDACS","STRDAEQ",
      "STRDAGE","STRDAGT","STRDAHI","STRDAHS","STRDALE","STRDALO","STRDALS","STRDALT","STRDAMI","STRDANE","STRDACC","STRDAPL",
      "STRDAVC","STRDAVS","STRDB","STRDBCS","STRDBEQ","STRDBGE","STRDBGT","STRDBHI","STRDBHS","STRDBLE","STRDBLO","STRDBLS",
      "STRDBLT","STRDBMI","STRDBNE","STRDBCC","STRDBPL","STRDBVC","STRDBVS","STREA","STREACS","STREAEQ","STREAGE","STREAGT","STREAHI",
      "STREAHS","STREALE","STREALO","STREALS","STREALT","STREAMI","STREANE","STREACC","STREAPL","STREAVC","STREAVS","STRED","STREDCS",
      "STREDEQ","STREDGE","STREDGT","STREDHI","STREDHS","STREDLE","STREDLO","STREDLS","STREDLT","STREDMI","STREDNE","STREDCC","STREDPL",
      "STREDVC","STREDVS","STREQ","STRFA","STRFACS","STRFAEQ","STRFAGE","STRFAGT","STRFAHI","STRFAHS","STRFALE","STRFALO","STRFALS",
      "STRFALT","STRFAMI","STRFANE","STRFACC","STRFAPL","STRFAVC","STRFAVS","STRFD","STRFDCS","STRFDEQ","STRFDGE","STRFDGT","STRFDHI",
      "STRFDHS","STRFDLE","STRFDLO","STRFDLS","STRFDLT","STRFDMI","STRFDNE","STRFDCC","STRFDPL","STRFDVC","STRFDVS","STRGE","STRGT",
      "STRHI","STRHS","STRIA","STRIACS","STRIAEQ","STRIAGE","STRIAGT","STRIAHI","STRIAHS","STRIALE","STRIALO","STRIALS","STRIALT",
      "STRIAMI","STRIANE","STRIACC","STRIAPL","STRIAVC","STRIAVS","STRIB","STRIBCS","STRIBEQ","STRIBGE","STRIBGT","STRIBHI","STRIBHS",
      "STRIBLE","STRIBLO","STRIBLS","STRIBLT","STRIBMI","STRIBNE","STRIBCC","STRIBPL","STRIBVC","STRIBVS","STRLE","STRLO","STRLS",
      "STRLT","STRMI","STRNE","STRCC","STRPL","STRVC","STRVS","EQU","DCD","DCB","FILL","SPACE","ADR","ADREQ","ADRNE","ADRMI","ADRPL",
      "ADRHI","ADRHS","ADRLO","ADRLS","ADRGE","ADRGT","ADRLE","ADRLT","ADRVS","ADRVC","ADRCC","ADRCS",],

    // we include these common regular expressions
    symbols: /[=!~?:&|+\-*\/\^%]+/,

    // C# style strings
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    // The main tokenizer for our languages
    tokenizer: {
      root: [
        // identifiers and keywords
        [/[a-z_$][\w$]*/, {
          cases: {
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }],

        // whitespace
        { include: '@whitespace' },

        // delimiters and operators
        [/[{}()\[\]]/, '@brackets'],
        //[/[<>](?!@symbols)/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'symbol.operator',
            '@default': 'symbol.other'
          }
        }],


        // numbers
      
        [/#-?0[xX][0-9a-fA-F][0-9a-fA-F_]*/, 'number.hash.hex'],
        [/#-?0[bB][0-1][01_]*/, 'number.hash.bin'],
        [/#-?\d[\d_]*/, 'number.hash'],
        [/-?0[xX][0-9a-fA-F][0-9a-fA-F_]*/, 'number.bare.hex'],
        [/-?0[bB][0-1][01_]*/, 'number.bare.bin'],
        [/-?\d[\d_]*/, 'number.bare'],

        // delimiter: after number because of .\d floats
        [/[,.]/, 'delimiter'],

        // strings
        [/"([^"\\]|\\.)*$/, 'string.invalid'],  // non-teminated string
        [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

        // characters
        [/'[^\\']'/, 'string'],
        [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
        [/'/, 'string.invalid'],

        // ARM comments
        [/;(.*)/, 'comment'],
        [/>>;(.*)/, 'comment.testpass'],
        [/>>-(.*)/, 'comment.testerror'],

      ],


      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
      ],

      whitespace: [
        [/[ \t\r\n]+/, 'white'],
        //        [/\/\*/, 'comment', '@comment'],
        //        [/\/\/.*$/, 'comment'],
      ],
    }
  });

  // Convert CSS-stle hex color (with #) to form needed by syntax highlighting.
  // Add a JS color display extension to have colors displayed in source
  function cs (color)  { 
    return color.substr(1);
  }

  var base03 = '#002b36';
  var base02 = '#073642';
  var base01 = '#586e75';
  var base00 = '#657b83';
  var base0 = '#839496';
  var base1 = '#93a1a1';
  var base2 = '#eee8d5';
  var base3 = '#fdf6e3';
  var yellow = '#b58900';
  var orange = '#cb4b16';
  var red = '#dc322f';
  var magenta = '#d33682';
  var violet = '#6c71c4';
  var blue = '#268bd2';
  var cyan = '#2aa198';
  var green = '#859900';

  monaco.editor.defineTheme('one-light-pro', {
    base: 'vs',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operator', foreground: cs('#303030')},
      { token: 'keyword', foreground: cs('#1010a0')},
      { token: 'symbols', foreground: cs('#303030')},
      { token: 'comment', foreground: cs('#308030')},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#ff0000')},
      { token: 'string', foreground: cs('#e06c75') },
      {token: 'number.bare', foreground: cs("#c08000") },
      { token: 'number.hash', foreground: cs("#408080")}
    ],
    "colors": {
      'editor.foreground': '#000000',
      'editor.background': '#EDF9FA',
      'editorCursor.foreground': '#8B0000',
      'editor.lineHighlightBackground': base2,
      'editorLineNumber.foreground': base1,
      'editor.selectionBackground': '#CC0080',
      'editor.inactiveSelectionBackground': base3
    }
  });

  monaco.editor.defineTheme('one-dark-pro', {
    base: 'vs-dark',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'operator', foreground: cs('#b0b0b0')},
      { token: 'keyword', foreground: cs(blue)},
      { token: 'symbol', foreground: cs('#a0a0a0')},
      { token: 'comment', foreground: cs('#20a020')},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2')},
      { token: 'string', foreground: cs('#e06c75')},
      { token: 'number.hash', foreground: cs("#80c0c0")},
      {token: 'number.bare', foreground: cs("#f0f080")}
    ],
    "colors": {
      'editor.foreground': '#FFFFFF',
      //'editor.background': '#000000',
      'editorCursor.foreground': '#8B0000',
      'editor.lineHighlightBackground': base02,
      'editorLineNumber.foreground': base01,
      'editor.selectionBackground': '#CC0080',
      'editor.inactiveSelectionBackground': base01,
      'editor.findMatchBackground': base00, // Color of the current search match.
      'editor.findMatchHighlightBackground':base02 // Color of the other search matches.
    }
  });

  
  monaco.editor.defineTheme('solarised-light', {
    base: 'vs',
    inherit: true, // can also be false to completely replace the builtin rules
    rules: [
      { token: 'delimiter', foreground: cs(base00)},
      { token: 'identifier', foreground: cs(base00)},
      { token: 'keyword', foreground: cs(blue)},
      { token: 'symbol', foreground: cs(base00)},
      { token: 'comment', foreground: cs(green)},
      { token: 'comment.testerror', foreground: cs(red) },
      { token: 'escape', foreground: cs('#57b6c2')},
      { token: 'string', foreground: cs('#e06c75')},
      { token: 'number.hash', foreground: cs(cyan)},
      {token: 'number.bare', foreground: cs(yellow)}
    ],
    "colors": {
      'foreground': base00,
      'editor.foreground': base00,
      'editor.background': base3,
      'editorCursor.foreground': magenta,
      'editor.lineHighlightBackground': base2,
      'editorLineNumber.foreground': base1,
      'editor.selectionBackground': base1,
      'editor.inactiveSelectionBackground': base1,
      'editor.findMatchBackground': base0, // Color of the current search match.
      'editor.findMatchHighlightBackground':base2 // Color of the other search matches.
    }
  });


    monaco.editor.defineTheme('solarised-dark', {
      base: 'vs-dark',
      inherit: true, // can also be false to completely replace the builtin rules
      rules: [
        { token: 'delimiter', foreground: cs(base0)},
        { token: 'identifier', foreground: cs(base0)},
        { token: 'keyword', foreground: cs(blue)},
        { token: 'symbol', foreground: cs(base0)},
        { token: 'comment', foreground: cs(green)},
        { token: 'comment.testerror', foreground: cs(red) },
        { token: 'escape', foreground: cs('#57b6c2')},
        { token: 'string', foreground: cs('#e06c75')},
        { token: 'number.hash', foreground: cs(cyan)},
        {token: 'number.bare', foreground: cs(yellow)}
      ],
      "colors": {
        'foreground': base0,
        'editor.foreground': base0,
        'editor.background': base03,
        'editorCursor.foreground': magenta,
        'editor.lineHighlightBackground': base02,
        'editorLineNumber.foreground': base01,
        'editor.selectionBackground': base01,
        'editor.inactiveSelectionBackground': base01,
        'editor.findMatchBackground': base00, // Color of the current search match.
        'editor.findMatchHighlightBackground':base02 // Color of the other search matches.
      }
    });
  

    //'foreground' // Overall foreground color. This color is only used if not overridden by a component.
//'errorForeground' // Overall foreground color for error messages. This color is only used if not overridden by a component.
//'descriptionForeground' // Foreground color for description text providing additional information, for example for a label.
//'focusBorder' // Overall border color for focused elements. This color is only used if not overridden by a component.
//'contrastBorder' // An extra border around elements to separate them from others for greater contrast.
//'contrastActiveBorder' // An extra border around active elements to separate them from others for greater contrast.
//'selection.background' // The background color of text selections in the workbench (e.g. for input fields or text areas). Note that this does not apply to selections within the editor.
//'textSeparator.foreground' // Color for text separators.
//'textLink.foreground' // Foreground color for links in text.
//'textLink.activeForeground' // Foreground color for active links in text.
//'textPreformat.foreground' // Foreground color for preformatted text segments.
//'textBlockQuote.background' // Background color for block quotes in text.
//'textBlockQuote.border' // Border color for block quotes in text.
//'textCodeBlock.background' // Background color for code blocks in text.
//'widget.shadow' // Shadow color of widgets such as find/replace inside the editor.
//'input.background' // Input box background.
//'input.foreground' // Input box foreground.
//'input.border' // Input box border.
//'inputOption.activeBorder' // Border color of activated options in input fields.
//'input.placeholderForeground' // Input box foreground color for placeholder text.
//'inputValidation.infoBackground' // Input validation background color for information severity.
//'inputValidation.infoBorder' // Input validation border color for information severity.
//'inputValidation.warningBackground' // Input validation background color for information warning.
//'inputValidation.warningBorder' // Input validation border color for warning severity.
//'inputValidation.errorBackground' // Input validation background color for error severity.
//'inputValidation.errorBorder' // Input validation border color for error severity.
//'dropdown.background' // Dropdown background.
//'dropdown.foreground' // Dropdown foreground.
//'dropdown.border' // Dropdown border.
//'list.focusBackground' // List/Tree background color for the focused item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
//'list.focusForeground' // List/Tree foreground color for the focused item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
//'list.activeSelectionBackground' // List/Tree background color for the selected item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
//'list.activeSelectionForeground' // List/Tree foreground color for the selected item when the list/tree is active. An active list/tree has keyboard focus, an inactive does not.
//'list.inactiveSelectionBackground' // List/Tree background color for the selected item when the list/tree is inactive. An active list/tree has keyboard focus, an inactive does not.
//'list.inactiveSelectionForeground' // List/Tree foreground color for the selected item when the list/tree is inactive. An active list/tree has keyboard focus, an inactive does not.
//'list.hoverBackground' // List/Tree background when hovering over items using the mouse.
//'list.hoverForeground' // List/Tree foreground when hovering over items using the mouse.
//'list.dropBackground' // List/Tree drag and drop background when moving items around using the mouse.
//'list.highlightForeground' // List/Tree foreground color of the match highlights when searching inside the list/tree.
//'pickerGroup.foreground' // Quick picker color for grouping labels.
//'pickerGroup.border' // Quick picker color for grouping borders.
//'button.foreground' // Button foreground color.
//'button.background' // Button background color.
//'button.hoverBackground' // Button background color when hovering.
//'badge.background' // Badge background color. Badges are small information labels, e.g. for search results count.
//'badge.foreground' // Badge foreground color. Badges are small information labels, e.g. for search results count.
//'scrollbar.shadow' // Scrollbar shadow to indicate that the view is scrolled.
//'scrollbarSlider.background' // Slider background color.
//'scrollbarSlider.hoverBackground' // Slider background color when hovering.
//'scrollbarSlider.activeBackground' // Slider background color when active.
//'progressBar.background' // Background color of the progress bar that can show for long running operations.
//'editor.background' // Editor background color.
//'editor.foreground' // Editor default foreground color.
//'editorWidget.background' // Background color of editor widgets, such as find/replace.
//'editorWidget.border' // Border color of editor widgets. The color is only used if the widget chooses to have a border and if the color is not overridden by a widget.
//'editor.selectionBackground' // Color of the editor selection.
//'editor.selectionForeground' // Color of the selected text for high contrast.
//'editor.inactiveSelectionBackground' // Color of the selection in an inactive editor.
//'editor.selectionHighlightBackground' // Color for regions with the same content as the selection.
//'editor.findRangeHighlightBackground' // Color the range limiting the search.
//'editor.hoverHighlightBackground' // Highlight below the word for which a hover is shown.
//'editorHoverWidget.background' // Background color of the editor hover.
//'editorHoverWidget.border' // Border color of the editor hover.
//'editorLink.activeForeground' // Color of active links.
//'diffEditor.insertedTextBackground' // Background color for text that got inserted.
//'diffEditor.removedTextBackground' // Background color for text that got removed.
//'diffEditor.insertedTextBorder' // Outline color for the text that got inserted.
//'diffEditor.removedTextBorder' // Outline color for text that got removed.
//'merge.currentHeaderBackground' // Current header background in inline merge-conflicts.
//'merge.currentContentBackground' // Current content background in inline merge-conflicts.
//'merge.incomingHeaderBackground' // Incoming header background in inline merge-conflicts.
//'merge.incomingContentBackground' // Incoming content background in inline merge-conflicts.
//'merge.commonHeaderBackground' // Common ancestor header background in inline merge-conflicts.
//'merge.commonContentBackground' // Common ancester content background in inline merge-conflicts.
//'merge.border' // Border color on headers and the splitter in inline merge-conflicts.
//'editorOverviewRuler.currentContentForeground' // Current overview ruler foreground for inline merge-conflicts.
//'editorOverviewRuler.incomingContentForeground' // Incoming overview ruler foreground for inline merge-conflicts.
//'editorOverviewRuler.commonContentForeground' // Common ancestor overview ruler foreground for inline merge-conflicts.
//'editor.lineHighlightBackground' // Background color for the highlight of line at the cursor position.
//'editor.lineHighlightBorder' // Background color for the border around the line at the cursor position.
//'editor.rangeHighlightBackground' // Background color of highlighted ranges, like by quick open and find features.
//'editorCursor.foreground' // Color of the editor cursor.
//'editorWhitespace.foreground' // Color of whitespace characters in the editor.
//'editorIndentGuide.background' // Color of the editor indentation guides.
//'editorLineNumber.foreground' // Color of editor line numbers.
//'editorRuler.foreground' // Color of the editor rulers.
//'editorCodeLens.foreground' // Foreground color of editor code lenses
//'editorBracketMatch.background' // Background color behind matching brackets
//'editorBracketMatch.border' // Color for matching brackets boxes
//'editorOverviewRuler.border' // Color of the overview ruler border.
//'editorGutter.background' // Background color of the editor gutter. The gutter contains the glyph margins and the line numbers.
//'editorError.foreground' // Foreground color of error squigglies in the editor.
//'editorError.border' // Border color of error squigglies in the editor.



  var mevent = new CustomEvent("monaco-ready", { "detail": "ready now!" });

  // Dispatch/Trigger/Fire the event
  document.dispatchEvent(mevent);
});





