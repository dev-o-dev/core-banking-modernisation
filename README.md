# MegaBank Core Banking Host (DDA / CIF / Payments)

The mainframe core banking platform that the **MegaBank Internet Banking
Channel** (`internet-banking-monolith`) calls "the host". It is written as
it would have looked in a UK retail bank around 2010. The oldest parts
date from the late 1980s, with layers added for Y2K, internet banking
(2000–2002), Faster Payments (2008) and a large 2009–2011 programme
that brought interest, charges, standing orders, direct debits, the
general ledger feed, statements, cheques, debit cards, personal loans,
ISAs and FX onto the host. Later additions brought customer self-service
channel services (2012: cheque stops, overdraft limits, statement
requests, contact preferences) and the Current Account Switch Service
(CASS, 2013), plus overdraft review, notice savings, customer merges,
customer segmentation, management information, a business continuity
extract, transaction history archiving and the Bank of England quarterly
return. From 2014 came LINK ATM settlement, an internet banking
transaction feed, branch paying-in and bank giro credits, a nightly
savings sweep, complaints redress payments, customer alerts, FATCA / CRS
reporting and GDPR subject access extracts.

It is a **modernisation starting point**, like the channel monolith. The
functionality is realistic but not complete, and the architectural smells
are intentional.

> **Source only.** The code targets IBM z/OS: Enterprise COBOL, CICS TS,
> DB2 for z/OS V9, VSAM, IBM MQ, JCL, DFSORT, CA-7 and Connect:Direct.
> It has not been compiled and will not run off-platform without an
> emulator. Each directory corresponds to a partitioned dataset (PDS)
> library on the host.

---

## Platform

| Layer | Technology |
|---|---|
| Hardware / OS | IBM System z, z/OS 1.11 |
| Language | Enterprise COBOL for z/OS 4.2 (plus one REXX exec) |
| Online | CICS TS 3.2, region `CICSPRD1`. BMS green screens for branch staff; DPL services for channels |
| Channel access | CICS Transaction Gateway (ECI) and MQ-CICS DPL bridge |
| Master files | VSAM KSDS: CIF master, DDA account master (+ alternate index by CIF), product parameters, standing orders (+ due-date AIX), DD mandates, funds holds, pending charges, pending rate changes, cheque stops / uncleared cheques, personal loans (+ AIX by CIF), FX rates, notice-savings withdrawal notices, CASS switches, ad-hoc statement requests, savings sweep instructions, customer alert preferences, FATCA / CRS self-certifications, account-number and CIF-number control files, the transaction-history archive checkpoint |
| Posting log | VSAM ESDS memo log + KSDS duplicate-reference file |
| Relational | DB2 for z/OS V9 (subsystem `DB2P`): transaction history, payment instructions, interest paid, charge history, GL mapping / control balances / suspense, AML alerts, loan transactions and letters, FX rate history |
| Messaging | WebSphere MQ 7 (queue manager `MQP1`) to the payments gateway; standby DPL bridge queues for the card gateway |
| Batch | JCL + DFSORT + IDCAMS, scheduled by CA-7 (networks `MBKEOD`, `MBKWKLY`, `MBKMTH`, `MBKQTR`) |
| File transfer | Connect:Direct to IBANK, the card processor and card scheme, the BACS bureau (PTX), SWIFT Alliance, IPSL (cheques), CASS via BACSTEL-IP, Oracle GL, Group Treasury (FX fixings), HMRC, Document Services, the data warehouse, the DR site and branch contingency PCs, Credit Risk MI, the DPO consent store, the CRM system, the complaints system (CMS) and the SMS / e-mail alert gateway |
| Source control | CA Endevor (not represented here) |

---

## Library layout

| Directory | Host library | Contents |
|---|---|---|
| `COBOL/` | `MBK.PRD.SOURCE` | COBOL programs: channel services, batch suites, branch screens, called subprograms |
| `COPYLIB/` | `MBK.PRD.COPYLIB` | Record layouts, COMMAREA, interface files, common routines |
| `DCLGEN/` | `MBK.PRD.DCLGEN` | DB2 table declarations and host variables |
| `DDL/` | `MBK.PRD.DDL` | DB2 database, tablespaces, tables, indexes, sequence, grants |
| `BMS/` | `MBK.PRD.BMS` | 3270 maps for the branch screens (CF01, CF02, DD03, DD04, SO01, DM01, LN01, FX01, CS01) |
| `CSD/` | `MBK.PRD.CSD` | CICS resource definitions (DFHCSDUP input) |
| `MQSC/` | `MBK.PRD.MQSC` | MQ queue / process definitions |
| `JCL/` | `MBK.PRD.JCL` | Production batch jobs and DBA jobs |
| `PROCLIB/` | `MBK.PRD.PROCLIB` | Standard batch step procedures |
| `CNTL/` | `MBK.PRD.CNTL` | IDCAMS cluster and GDG definitions, DFSORT, BIND control, date card, bank holidays |
| `CDPROC/` | `MBK.PRD.CDPROC` | Connect:Direct process |
| `REXX/` | `MBK.PRD.REXX` | Business date roll |
| `SCHED/` | (CA-7 database) | End-of-day, weekly, monthly and quarterly network extract |

Naming: 8-character members. The prefix gives the subsystem: `CF` = CIF,
`DD` = deposit accounts, `PS` = posting, `PY` = payments, `CH` = channel
interface, `IB` = internet banking feed, `CR` = cards interface, `CD` =
card settlement, `ST` = statements, `RG` = regulatory, `MG` = mortgages,
`IN` = interest, `CG` = charges, `SO` = standing orders, `DM` = direct
debit mandates, `GL` = general ledger, `CQ` = cheques, `LN` = personal
loans, `IS` = ISAs, `FX` = foreign exchange, `PR` = product parameters,
`OD` = overdrafts, `CSS` = current account switching, `MI` = management
information, `BCP` = business continuity, `SV` = savings, `CM` =
complaints, `ATM` = ATM network, `TAX` = tax reporting, `SAR` = subject
access requests.
Most batch programs end in `010`/`020`/`030`, most CICS programs in
`01`, and most jobs in `01`. The `MBK` prefix is used
for shared and infrastructure members. Datasets are
`MBK.PRD.<subsystem>.<name>`.

---

## Components

Numbers refer to the component list in the channel repo's
`docs/core-banking-components.md`; `–` marks capabilities added on the
host that the list does not number.

| # | Component | Status | Programs / artefacts |
|---|---|---|---|
| 1 | Customer Information File (CIF) | Built | `CFCUSREC`, `CFINQ01`, `CFMNT01` (online maintenance, CIF number allocation), `CFMNT201`, `CFSCRN1`/`CFSCRN2`, `CFDEFCIF`, `CFDEFCTL` |
| – | Bereavement | Built | `CFDEC010` (`CFDEC01`): freezes the deceased's accounts, cancels SOs and mandates, transfers or pays out balances |
| – | KYC review, duplicates and merges | Built | `CFKYC010` (`CFKYC01`, weekly), `CFDUP010` (`CFDUP01`, monthly), `CFMRG010` (`CFMRG01`, weekly: merges four-eyes approved duplicates, moves accounts and loans to the kept CIF) |
| – | Customer segmentation | Built | `CFSEG010` (`CFSEG01`, monthly: sets the CIF segment - Premier, Plus, Student, Basic, Dormant and so on - from deposit and loan holdings, sends changes to CRM for relationship manager assignment) |
| – | Contact preferences | Built | `CFPRF01` (service `CUSTPREF`: contact details, marketing consent, e-statement opt-in, statement cycle; audit TDQ CPAU swept by `CPAUSWP1`) |
| 2 | Deposit accounts (DDA) | Built | `DDACCREC`, `DDINQ01`, `DDLST01`, `DDTXN01` (DB2 history), `DDDEFACM` |
| – | Account lifecycle | Built | `DDOPN01` (opening, number allocation with `DDMODCK`/`DDIBAN1`), `DDCLS01` + `DDCLS010` (two-night closure), `DDDRM010` (dormancy, Reclaim Fund) |
| – | Arranged overdrafts | Built | `ODLIM01` (service `ODLIMIT`: enquire / request / remove with affordability, referral to branch; audit TDQ ODAU swept by `ODAUSWP1`), `ODREV010` (`ODREV01`: expiry notices, removal of expired limits, referrals to the Financial Support Unit), `ODLTRREC` |
| – | Current account switching (CASS) | Built | `CSSVC01` (service `SWITCHRQ`, switch-in requests), `CSSIN010` / `CSSOU010` (`CSSDLY01`: CASS inbound and outbound messages, SO / mandate transfer, closing balance transfer, redirection), `CSSRECS`, `CSDEFSWT`; branch screen `CSSCRN1` (CS01, links `CSSVC01`) |
| – | Funds holds | Built | `DDHLD01` (online), `DDHLX010` (expiry), `DDHLDREC`, `DDSCRN4` (DD04) |
| – | Product parameters | Built | `PRDPARRC`, `PRMNT010` (`PRMNT01` change sets) |
| – | Interest | Built | `INACR010` (daily accrual), `INCAP010` (capitalisation and tax), `INRAT010` (rate changes with notice), `INFTD010` (fixed-term maturities), `INTAX010` (certificates and HMRC BBSI) |
| – | Savings: notice accounts | Built | `INNTC010` (`INNTC01`: pays matured withdrawal notices, early-withdrawal penalty, closure on notice), `NTCWDREC`, `INDEFNTC` (CICS file SAVNTC) |
| – | Savings: sweep | Built | `SVSWP010` (`SVSWP01`: nightly sweep of current account excess to savings and back below a floor, on the projected balance after tonight's queued debits; `SWPPOST` feed), `SVSWPREC` (CICS file SAVSWP) |
| – | Customer alerts | Built | `DDALR010` (`DDALR01`: unarranged overdraft, charge warning and low balance SMS / e-mail alerts to the alert gateway, CMA Retail Banking Order 2017), `ALRTPREC` (CICS file ALRTPRF, IBANK "manage alerts") |
| – | Complaints redress | Built | `CMPRD010` (`CMPRD01`: pays redress approved in CMS - refund, consequential loss, 8% simple interest net of tax - with letters, GL expense journal and a status file back to CMS; `RDRPOST` feed), `CMRDRREC` |
| 5 | Mortgage servicing | Stub | `MGSTUB1` returns "not available on this platform" |
| 6 | Posting engine | Built | `PSMEMO1`, `PSEOD010`, `PSRCN010` (memo reconciliation), `PSREV01` (same-day reversal), `PSTXNREC`, `PSXREFRC`, `TXNHIST`, `PSEOD01` with batch posting feeds; `PSARC010` (`PSARC01`, monthly: archives `TXNHIST` rows past 7 years to tape and deletes them, checkpoint/restart via `PSDEFCKP`), `PSARCREC` |
| – | Charges | Built | `CGCAL010`, `CGNTF010` (14-day pre-notification), `CGAPL010`, `CGREF010` (refunds), `CHGHIST` table; `CHGTAR10` (`CHGTAR01`, monthly: annual summary of charges and interest for personal current accounts on their opening anniversary), `CHGTARRC` |
| – | Standing orders | Built | `SOMNT01` (service), `SOSCRN1` (SO01), `SOEXE010`, `SORPT010` |
| – | Direct debits | Built | `DMSVC01` (service), `DMSCRN1` (DM01), `DMAUD010` (AUDDIS), `DMADC010` (ADDACS), `DMCOL010` (collections), `DMARU010` (ARUDD) |
| – | Cheques | Built | `CQCLR010` (inward clearing, pay/no-pay), `CQUNC010` (2-4-6 deposit clearance), `CQSTP01` (service `CHQSTOP`: place / remove / list stops, stop fee), `CHQRECS` |
| – | Branch paying-in and bank giro credits | Built | `PYBGC010` (`PYBGC01`: credits paying-in slips and bank giro credits from the branch image capture file, holds cheques as uncleared, outward clearing file, slip differences to branch suspense; `BGCPOST` feed) |
| 11 | Payments & clearing interfaces | Built | `PYINIT1`, `PYFPRSP`, `PYINB010`, `PYBAC010`, `PYSWF010`, `PAYINSTR` table |
| – | Debit cards | Built | `CRDAUT01` (real-time authorisation with holds), `CDSTL010` (scheme settlement), `CRDM0101` (card maintenance feed) |
| – | LINK ATM settlement | Built | `ATMSTL10` (`ATMSTL01`: daily LINK settlement - cardholder cash withdrawals and ATM fees, reversals, acquirer interchange, net settlement check, GL journal `ATMS`; `ATMPOST` feed), `ATMLKREC` |
| 13 | Statements | Built | `STSTM010` (select), `STSTM020` (compose from `TXNHIST`), `STSTM030` (update, e-statement feed), `STREQ01` (service `STMTREQ`: ad-hoc and duplicate statements via CICS file STMTADH, produced by that night's `STMT01`), `STREQREC`, `STDEFADH`; printing is in Document Services |
| – | General ledger | Built | `GLEXT010` (customer movements via `GLMAP`), `GLREC010` (validation and sub-ledger reconciliation), `GLSUS010` (suspense ageing), `GLJNLREC`, `MBKGLMAP.sql` seed |
| 15 | Branch screens | Built | CF01, CF02, DD03, DD04, SO01, DM01, LN01, FX01, CS01 (see below) |
| 16 | Channel integration layer | Built | `CHROUTER` + `CHCOMMAR`/`CHCOMMA2` (CTG / IPIC / MQ bridge entry point, 22 services), CSD `MBKCORE` + `MBKCOR2` + `MBKCOR3`, MQSC |
| – | Channel usage reporting | Built | `CHLGRPT1` (`CHLGSWP1`: sweeps the `CHROUTER` audit log TDQ CHLG daily and reports calls, return codes and response times per service and channel against the 2-second IBANK SLA) |
| 17 | Batch extracts & feeds | Built | `IBFD0201` (IBFEED02), `IBFD0301` (IBFEED03: transaction feed for the IBANK search and spending analysis shadow table, layout `IBFD3REC`), `CFMNT201` (CUSTMNT2), `CRDM0101` (CRDMNT1) |
| – | Management information | Built | `MIEXT010` (`MIEXT01`, monthly: deposit MI by product, branch and segment for the data warehouse) |
| – | Business continuity | Built | `BCPEXT10` (`BCPEXT01`, nightly, last job before the date roll: account and spending-power extract for the branch BCPVIEW contingency PCs, split by region, and for the DR site), `BCPEXREC` |
| 18 | EOD batch & scheduling | Built (supporting) | `MBKEOD.ca7`, `MBKONLDN`/`MBKONLUP`, `MBKDTROL` + `MBKDTRX`, `DATECARD`, `DTEUTIL`, `MBKDEFVS` |
| – | Personal loans | Built | `LNBKG010` (booking/drawdown), `LNINT010`, `LNREP010`, `LNARR010` (arrears, CCA notices, collections), `LNSTM010` (CCA s77A statements), `LNSVC01` + `LNSCRN1` (LN01) |
| – | Savings: cash ISA | Built | `ISASUB10` (subscription monitoring, HMRC year-end return) |
| 21 | Regulatory | Built | `RGSCV020` (FSCS Single Customer View with balances, run by `RGSCV01`), `RGAML010` (AML rules R01–R07), `RGBOE010` (`RGBOE01`: Bank of England quarterly statistical return, sterling deposit and lending lines), `RGBOEREC`; the `RGSCV010` stub is retired but still in the library |
| – | FATCA / CRS reporting | Built | `TAXCR010` (`TAXCR01`, annual on request: HMRC FATCA / CRS return with indicia search and USD 50,000 FATCA exemption, self-certification chase list), `TXRESREC` |
| – | GDPR subject access | Built | `SARXT010` (`SARXT01`: one printed pack per data subject access request logged by the DPO on ISPF panel `MBKSAR1` - customer, accounts, loans, SOs, DDs, six years of transactions - with statutory deadlines) |
| – | Foreign exchange | Built | `FXRAT010` (Treasury fixing load), `FXSVC01` (quote/validate, used by `PYINIT1`), FX01 rate board and quote (`FXSCRN1`) |

---

## How it fits together

### Online (CICS region `CICSPRD1`)

```
 IBANK channel (JBoss)                 Branch 3270 terminal
        |  CTG ECI / MQ bridge                 |  CF01
        v                                      v
   +----------+   LINK by service code    +---------+
   | CHROUTER |------------------------+  | CFSCRN1 |
   +----------+                        |  +---------+
     | audit -> TDQ CHLG (CHLGSWP1)    |
     v                                 v
  CIFINQ   -> CFINQ01 --READ------> CIFMAST (VSAM KSDS)
  ACCTINQ  -> DDINQ01 --READ------> ACCTMST (VSAM KSDS)
  ACCTLIST -> DDLST01 --BROWSE----> ACCTCIF (AIX path by CIF)
  MEMOPOST -> PSMEMO1 --UPDATE----> ACCTMST, PSTXREF, MEMOLOG
  PAYINIT  -> PYINIT1 --LINK PSMEMO1, INSERT PAYINSTR (DB2),
                        MQPUT1 MBK.FPS.OUTBOUND / MBK.CHAPS.OUTBOUND
  MORTINQ  -> MGSTUB1 (stub)
  ... plus the 2010-2013 services in the table below

  MBK.FPS.RESPONSE --trigger--> PYFR / PYFPRSP --> PAYINSTR ACK/REJ,
                                                   reversal via PSMEMO1

 Card gateway --IPIC MBKCARD, mirror CHMC (TCLASS MBKCRD)--> CHROUTER
              --standby: MQ bridge MBK.CARD.BRIDGE.REQ ------^
```

#### Channel services (`CHROUTER` service table)

| Service | Program | What it does | Main resources |
|---|---|---|---|
| `CIFINQ` | `CFINQ01` | Customer enquiry | CIFMAST |
| `ACCTINQ` | `DDINQ01` | Account balance enquiry | ACCTMST |
| `ACCTLIST` | `DDLST01` | Accounts for a customer | ACCTCIF (AIX path) |
| `MEMOPOST` | `PSMEMO1` | Real-time memo posting | ACCTMST, PSTXREF, MEMOLOG |
| `PAYINIT` | `PYINIT1` | FPS / CHAPS / BACS / SWIFT initiation (SWIFT rate checked by `FXSVC01`) | PAYINSTR (DB2), MQ |
| `MORTINQ` | `MGSTUB1` | Mortgage enquiry (stub) | – |
| `SOMAINT` | `SOMNT01` | Standing order list / add / amend / cancel | SOMAST |
| `DDMANDT` | `DMSVC01` | Direct debit mandate list / cancel (ADDACS to originator) | DDMAND |
| `ACCTOPEN` | `DDOPN01` | Account opening, number + check digit + IBAN | ACCTMST, ACCTNOC, PRDPARM |
| `ACCTCLOS` | `DDCLS01` | Closure quote and request (completed by `DDCLS010`) | ACCTMST, CHGPEND |
| `ACCTHOLD` | `DDHLD01` | Place / list / release funds holds | DDHOLD, ACCTMST |
| `TXNHIST` | `DDTXN01` | Transaction history from DB2 | TXNHIST (plan MBKCICS1/2) |
| `CIFMAINT` | `CFMNT01` | Customer create / amend, CIF number allocation, audit TDQ CFAU | CIFMAST, CIFCTL |
| `POSTREV` | `PSREV01` | Same-day reversal of a memo posting | via PSMEMO1 |
| `CARDAUTH` | `CRDAUT01` | Debit card authorise / reverse, places card holds (CARDS channel only) | ACCTMST, via DDHLD01 |
| `LOANENQ` | `LNSVC01` | Personal loan enquiry and settlement quote | LOANMST, LOANCIF |
| `FXQUOTE` | `FXSVC01` | FX quote / rate validation | FXRATES |
| `CHQSTOP` | `CQSTP01` | Cheque stops: place (single or range) / remove (branch or TELE only) / list; stop fee via `PSMEMO1` | CHQSTOP, ACCTMST, PRDPARM |
| `ODLIMIT` | `ODLIM01` | Arranged overdraft enquire / request / change / remove, affordability check, refer to branch | ACCTMST, CIFMAST, PRDPARM, TDQ ODAU |
| `STMTREQ` | `STREQ01` | Ad-hoc or duplicate statement request, list / cancel waiting requests; duplicate fee | STMTADH, ACCTMST, CIFMAST, PRDPARM |
| `CUSTPREF` | `CFPRF01` | Contact details, marketing consent, e-statement opt-in, statement cycle | CIFMAST, ACCTCIF, ACCTMST, PRDPARM, TDQ CPAU |
| `SWITCHRQ` | `CSSVC01` | CASS switch-in request / enquire / cancel (before the request is sent) | SWITCHES, ACCTMST, CIFMAST |

The 2010-2013 services use the request/response layouts in `CHCOMMA2`;
`CHCOMMAR` itself is unchanged at version `01`. The five 2012/2013
services (`CHQSTOP` to `SWITCHRQ`) are defined in CSD group `MBKCOR3`.

#### Branch screens

| Transaction | Program | Map | Function |
|---|---|---|---|
| CF01 | `CFSCRN1` | `CFMAPS` | Customer enquiry |
| CF02 | `CFSCRN2` | `CFMAP2` | Customer account list |
| DD03 | `DDSCRN3` | `DDMAP3` | Transaction history (links `DDTXN01`, DB2ENTRY `MBKTXH`) |
| DD04 | `DDSCRN4` | `DDMAP4` | Funds holds: place / release / list (links `DDHLD01`) |
| SO01 | `SOSCRN1` | `SOMAPS` | Standing order maintenance (links `SOMNT01`) |
| DM01 | `DMSCRN1` | `DMMAPS` | Direct debit mandate list / cancel (links `DMSVC01`) |
| LN01 | `LNSCRN1` | `LNMAPS` | Personal loan enquiry / settlement quote (links `LNSVC01`) |
| FX01 | `FXSCRN1` | `FXMAPS` | FX rate board and quote (links `FXSVC01`) |
| CS01 | `CSSCRN1` | `CSMAPS` | CASS current account switch: request / enquire / cancel, list switches for an account (links `CSSVC01`; CSD group `MBKCOR3`) |

Apart from CF01/CF02 (direct file reads), the screens LINK to the same
service programs as the channels, so branch and IBANK rules stay the
same.

Every service uses the 4,000-byte `CHCOMMAR` COMMAREA: a common header
(eyecatcher, version, service code, channel, user, request id, return and
reason codes) followed by a service-specific layout. Return codes are
`0000` OK, `0004` warning, `0008` business reject, `0012` system error and
`0016` invalid request.

### Balances

The account master holds three balances:

- **Ledger:** as at the last end-of-day hard posting.
- **Available:** cleared funds, which move in real time with memo postings.
- **Overdraft limit.**

The channel's "available balance" is available + overdraft. `IBFD0201`
does that addition when it builds the feed.

### End of day (CA-7 network `MBKEOD`)

```
MBKONLDN -> CUSTMNT2 ------------------------------------------------+
         -> PYINB01 -> PSEOD01 -> IBFEED02, IBFEED03 ------------------+
   CFDEC01 (19:00) -->   |     -> DDCLS01 -> STMT01 -------------------+
   INNTC01 ---------->   |     -> INACR01 -> INCAP01 -> INFTD01 -------+
   CSSDLY01 (CASS) -->   |               -> CGCHG01 -> CGREF01         |
   SVSWP01 (sweep) -->   |                          -> ODREV01 --------+
   PYBGC01, ATMSTL01,    |     -> SOEXE01, DMCOL01, ISASUB01, RGAML01 -+
   CMPRD01 (files) -->   |     -> LNDLY01, DDALR01, SARXT01 -----------+
                         +---- (GL journals) -> GLFEED01 -> GLSUS01 ---+
 00:30 CHLGSWP1 ------------------------------------------------------+
                                                                       v
                                     BCPEXT01 -> MBKDTROL -> MBKONLUP
 Event-triggered after MBKONLDN: DMDDI01 (AUDDIS), CQCLR01 (IPSL
 inward cheques), CDSTL01 (card scheme clearing), CSSDLY01 (CASS file),
 PYBGC01 (branch images), ATMSTL01 (LINK), CMPRD01 (CMS redress)
 00:40 ODAUSWP1, CPAUSWP1 sweep the online audit queues
```

Jobs that produce postings (interest, charges, SOs, DDs, closures,
cheques, cards, loans, bereavement, notice withdrawals, CASS switches,
savings sweeps, complaints redress, LINK ATM withdrawals, branch paying-in
credits) write them to fixed-name batch
posting feeds `MBK.PRD.PST.FEED.*`. The next `PSEOD01` sorts them in with
the memo log and hard posts them, so most of tonight's batch postings
reach the ledger tomorrow night. The feeders that run before `PSEOD01`
(`CFDEC01`, `INNTC01`, `CSSDLY01`, `SVSWP01`, `CMPRD01`, `ATMSTL01`,
`PYBGC01`) are posted the same night.

| Job | Programs | When | Purpose |
|---|---|---|---|
| `INACR01` | `INACR010` | daily after PSEOD01 | Credit and debit interest accrual |
| `INCAP01` | `INCAP010` | daily after INACR01 | Capitalise interest, deduct tax, `INTPAID` rows |
| `INFTD01` | `INFTD010` | daily after INCAP01 | Fixed-term deposit maturity notices and actions |
| `INRAT01` | `INRAT010` | on demand (Treasury) | Announce, then apply, rate changes |
| `INTAX01` | `INTAX010` | on demand after 5 April | Interest certificates and HMRC BBSI return |
| `CGCHG01` | `CGCAL010`, `CGNTF010`, `CGAPL010` | daily after INACR01 | Charges: calculate, notify (14 days), apply |
| `CGREF01` | `CGREF010` | daily after CGCHG01 | Dual-authorised charge refunds |
| `SOEXE01` | `SOEXE010`, `SORPT010` | daily after PSEOD01 | Pay due standing orders (internal / FPS) |
| `DMDDI01` | `DMAUD010`, `DMADC010` | on AUDDIS file arrival | Mandate set-up and ADDACS |
| `DMCOL01` | `DMCOL010`, `DMARU010` | daily after PSEOD01 | Pay or return direct debits (ARUDD) |
| `DDCLS01` | `DDHLX010`, `DDCLS010` | daily after PSEOD01 | Expire holds; two-night account closure |
| `DDDRM01` | `DDDRM010` | first Saturday (`MBKMTH`) | Dormancy and Reclaim Fund candidates |
| `STMT01` | `STSTM010`/`020`/`030` | daily after DDCLS01 | Statement cycle and e-statement feed |
| `CQCLR01` | `CQCLR010`, `CQUNC010` | on IPSL file arrival | Inward cheque decisions; 2-4-6 clearance |
| `CDSTL01` | `CDSTL010` | on scheme file arrival | Post settled card items, release holds |
| `LNDLY01` | `LNBKG010`, `LNINT010`, `LNREP010`, `LNARR010`, `LNSTM010` | daily after PSEOD01 | Personal loans daily cycle |
| `ISASUB01` | `ISASUB10` | daily after PSEOD01 | ISA subscription limit checks |
| `ISAYRE01` | `ISASUB10` | on demand, April | ISA year end and HMRC return |
| `GLFEED01` | `GLEXT010`, `GLREC010` | after all journal sources | GL feed to Oracle GL |
| `GLSUS01` | `GLSUS010` | daily after GLFEED01 | GL suspense ageing |
| `RGAML01` | `RGAML010` | daily after PSEOD01 | AML transaction monitoring |
| `CFDEC01` | `CFDEC010` | 19:00, held until MBKONLDN | Bereavement processing |
| `CFKYC01` | `CFKYC010` | Sunday 06:00 (`MBKWKLY`) | Periodic KYC review scheduling |
| `CFDUP01` | `CFDUP010` | first Saturday (`MBKMTH`) | Duplicate customer detection |
| `PRMNT01` | `PRMNT010` | on demand (Product Control) | Product parameter change sets |
| `FXRAT01` | `FXRAT010` | 07:45 and 14:15 on file arrival | Load Treasury FX fixing |
| `RGSCV01` | `RGSCV020` | on request (FSCS / PRA, within 72 hours), quarterly test | FSCS Single Customer View file |
| `INNTC01` | `INNTC010` | daily after MBKONLDN, before PSEOD01 | Pay matured notice-savings withdrawals (`NTCPOST` feed) |
| `CSSDLY01` | `CSSIN010`, `CSSOU010` | on CASS file arrival, before PSEOD01 | CASS daily cycle; outbound file by 02:00 (`CSSPOST` feed) |
| `ODREV01` | `ODREV010` | daily after PSEOD01 and CGCHG01 | Overdraft expiry, review letters, FSU referrals |
| `CHLGSWP1` | `CHLGRPT1` | 00:30 daily | Sweep channel audit log, usage and response-time report |
| `ODAUSWP1` / `CPAUSWP1` | IDCAMS, `MBKOPCMD` | 00:40 daily | Sweep TDQs ODAU / CPAU to Credit Risk MI / DPO consent store |
| `BCPEXT01` | `BCPEXT10` | nightly, last job before MBKDTROL | Business continuity extract (BCPVIEW regions, DR site) |
| `CFMRG01` | `CFMRG010` | Saturday 20:00 (`MBKWKLY`) | Merge approved duplicate customers |
| `MIEXT01` | `MIEXT010` | first business day (`MBKMTH`) | Monthly deposit MI to the data warehouse |
| `CHGTAR01` | `CHGTAR10` | first business day (`MBKMTH`) after CGCHG01 | Annual summary of charges letters |
| `PSARC01` | `PSARC010` | first Sunday after month end (`MBKMTH`) | Archive and purge `TXNHIST` (7-year retention) |
| `RGBOE01` | `RGBOE010` | first business day after quarter end (`MBKQTR`), before PSEOD01 | Bank of England quarterly statistical return |
| `CFSEG01` | `CFSEG010` | first Saturday (`MBKMTH`), before MIEXT01 | Customer segmentation, changes to CRM |
| `SVSWP01` | `SVSWP010` | daily after MBKONLDN (and CQCLR01 / CDSTL01), before PSEOD01 | Savings sweep (`SWPPOST` feed) |
| `CMPRD01` | `CMPRD010` | on CMS redress file arrival, before PSEOD01 | Pay complaints redress (`RDRPOST` feed, GL journal `CMPL`) |
| `ATMSTL01` | `ATMSTL10` | on LINK settlement file arrival, before PSEOD01 | LINK ATM settlement (`ATMPOST` feed, GL journal `ATMS`) |
| `PYBGC01` | `PYBGC010` | on branch image file arrival, before CQCLR01 and PSEOD01 | Branch paying-in slips and bank giro credits (`BGCPOST` feed, GL journal `BGCS`) |
| `DDALR01` | `DDALR010` | daily after PSEOD01 | Unarranged overdraft, charge warning and low balance alerts |
| `IBFEED03` | `IBFD0301` | daily after PSEOD01, alongside IBFEED02 | IBANK transaction shadow table feed |
| `SARXT01` | `SARXT010` | after PSEOD01 when the DPO has logged requests | GDPR subject access request packs |
| `TAXCR01` | `TAXCR010` | on request (Finance Tax), January | FATCA / CRS return to HMRC and self-certification chase list |

`PYBACS01` (20:30), `PYSWFT01` (four times a day), `FXRAT01`,
`INTAX01`, `RGBOE01`, `RGSCV01` and `TAXCR01` run outside the network. See `SCHED/MBKEOD.ca7` for times,
triggers, SLAs and return-code handling. VSAM clusters and GDG bases are
defined by `JCL/MBKDEFVS` from the `CNTL` members (`*DEF*`, `PSDEFCKP`, `MBKGDG2`);
DB2 packages and plans are in `CNTL/MBKBIND`.

---

## Interfaces with the internet banking channel

| Interface | Direction | Host side | Channel side (`internet-banking-monolith`) |
|---|---|---|---|
| Channel services | channel → host, real time | `CHROUTER` / `CHCOMMAR` + `CHCOMMA2` (22 services) | Not wired: the channel posts to its own shadow tables (see its `docs/architecture.md`) |
| `IBFEED02` | host → channel, nightly | `IBFD0201`, layout `IBFEEDRC` | Replaces `ACCT_MASTER` balances and appends `ACCT_TXN` |
| `IBFEED03` | host → channel, nightly | `IBFD0301`, layout `IBFD3REC` | Loads the transaction shadow table `IBK_TXN_SHADOW` (transaction search, spending categories) |
| `CUSTMNT2` | channel → host, nightly | `CFMNT201`, layout `CUSTMNRC` | Changes to `CUST_MASTER` (address, contact, employment, marketing) |
| `CRDMNT1` | channel → host → card processor | `CRDM0101`, layouts `CRDMNTRC` → `CMSMNTRC` | Changes to `CARD_MASTER` (freeze, lost/stolen, limits, switches) |

Field widths in `CFCUSREC`, `DDACCREC` and `IBFEEDRC` match the channel's
`sql/01-schema.sql`. The channel stores sort codes as `40-12-34`; the host
stores them as `401234`.

---

## Known legacy characteristics (intentional)

These are what a modernisation exercise should find:

- **Batch-window unavailability.** Payments and updates are unavailable
  from about 23:30 to 03:30 while the VSAM files are closed in CICS
  (`MBKONLDN`/`MBKONLUP`).
- **Posting split across two stores.** VSAM balances and DB2 history are
  updated without a coordinated commit. `PSEOD010` restart needs a VSAM
  restore plus a manual DB2 delete.
- **Duplicate check ordered after business rules.** In `PSMEMO1`, a
  resubmitted posting can be rejected as "NSF" instead of "DUPL"
  (PIR 2006/044).
- **Inbound Faster Payments applied only at end of day** (`PYINB010`).
- **Lost/stolen cards not blocked until 22:45**, because `CRDMNT1` runs
  as a nightly batch (RA-2006-031).
- **Bank holidays hard-coded twice**, in `DTEUTIL` and `CNTL(BANKHOL)`,
  and maintained by annual change request.
- **Annual DB2 partition rotation.** `TXNHIST` partitions must be added
  every year or `PSEOD010` fails in January (PIR 2008/112).
- **Entitlement not checked by the host.** Any authenticated channel can
  query any account (IA-IBK-002).
- **FX rate trusted from the channel** for SWIFT payments until 2011,
  when `PYINIT1` started validating it through `FXSVC01` (PIR 2009/201,
  closed). The old path is still in the code.
- **BACS payments go out in the bank's name, not the payer's**
  (PIR 2009/078).
- **Hard-coded routing tables:** service table in `CHROUTER`, channel
  whitelist, scheme limits, CHAPS cut-off.
- **COMMAREA versioning by hand.** `CHCOMMAR` is at version `01`, with
  4,000-byte REDEFINES for every service and Java classes generated from
  it.
- **Stubs left in production:** `MGSTUB1`. `RGSCV01` now runs the full
  SCV program `RGSCV020` (step `RG020`); the old `RGSCV010` stub is
  retired but still in the source library.
- **Postings a day late.** Batch suites write to posting feeds that the
  *next* `PSEOD01` applies, so interest, charges, SOs and loan
  repayments reach the ledger one night after they are calculated.
- **Batch window grows with every suite.** `MBKONLDN` now closes 20 CICS
  files (including SAVNTC, CHQSTOP, STMTADH, SWITCHES, SAVSWP and ALRTPRF) so batch can
  open them `DISP=OLD`. Jobs outside the window still need files closed
  by hand: `CFKYC01` (Sunday) and `CFMRG01` (Saturday 20:00) update
  CIFMAST, ACCTMAST and LOANMAST while CICS has them open (see the notes
  in `SCHED/MBKEOD.ca7`).
- **Archive by delete.** `TXNHIST` is purged row by row by `PSARC010`
  (no partition rotation, CR 14/0220 not funded); the DBA must reorg
  afterwards (PIR 2014/077), and a restarted run can archive the same
  rows twice (PIR 2012/031).
- **Consent and paperless coupled.** The CIF has no paperless flag, so
  `CFPRF01` only accepts an e-statement opt-in with e-mail marketing
  consent (PIR 2009/233).
- **Regulatory return as at the run date.** `RGBOE010` reads current
  balances, so if `RGBOE01` slips past the first business day the return
  includes new-quarter postings (PIR 2009/054).
- **Joint accounts.** The host holds one CIF per account (PIR 1999/007),
  so the SCV file cannot split joint balances, `DDOPN01` cannot open
  joint accounts, `CFSEG010` counts joint balances for the primary CIF
  only (PIR 2010/041) and `SARXT010` subject access packs miss the second
  holder (PIR 2018/077).
- **Overloaded fields.** `DDCLS010` marks "closing postings raised" in
  `ACM-DORMANCY-IND` because the account record had no spare field.
- **Copybook headers that disagree with the JCL.** `PRDPARRC` says
  `MBK.PRD.PRD.PARAMS` and `DDNOCREC` says `MBK.PRD.DDA.ACCTNOCTL`. The
  JCL, CSD and IDCAMS members use `PARMS` and `ACCTNO.CONTROL`.
- **GL gaps.** `GLMAP` skips `CHG` because charges are journaled at
  source, but the online SWIFT/CHAPS fees (`PYINIT1`) and non-sterling
  card fees (`CDSTL010`) are not journaled anywhere. Reversals of
  external payments go through the transfer clearing account, which then
  does not net to zero (see `DDL/MBKGLMAP.sql`).
- **No automated tests**, as in the channel.

## Not included

- Statement printing (`STMT02`/`STMT03`, Document Services library).
- The loan origination system (`LENDPRO` replacement) - approved loans
  arrive as a file for `LNBKG010`.
- Bank of England form BT-FX (non-sterling balances), which Treasury
  prepares by hand; `RGBOE010` builds the sterling lines only.
- The DBA reorg job `MBKREOTX` run after `PSARC01`, the archive reload
  field spec `CNTL(TXNHRELD)`, and the Connect:Direct jobs
  `MBKBCPCD`/`MBKBCPBR` that send the BCP files.
- Mortgage servicing (`MGSTUB1` only).
- The account-number range load member `CNTL(DDNOCLOD)`, the GDG
  bases for the original jobs (`MBKGDG1`) and the conversion load
  `DMCNV01`, which other members refer to.
- The in-house utility `MBKOPCMD` and the Endevor processors.
- CTG, DB2CONN, IPIC and MQCONN definitions (the `MBKCTG`, `MBKDB2` and
  `MBKMQ` CSD groups, and connection `MBKCARD`).
