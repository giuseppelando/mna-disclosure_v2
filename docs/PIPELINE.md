# M&A Disclosure Pipeline - Guida Completa

## Panoramica

Questa pipeline prepara i dati per l'analisi della relazione tra qualità informativa della disclosure pre-deal (10-K) e outcome delle operazioni M&A (premium, completion).

## Struttura dei Dati

### Unità di Analisi
- **Deal**: una singola operazione M&A identificata da `target_name` + `date_announced`
- **Filing**: un singolo 10-K identificato da `accession_number`

### Relazioni
- Un deal → uno (o zero) filing (dopo deduplication)
- Un filing ← molti deal (stessa azienda, deal diversi)

---

## Pipeline Completa - Ordine di Esecuzione

### PRE-PIPELINE: CIK Matching (una tantum o quando cambia il campione SDC)

#### Step 0a: Match automatico CIK
```r
source("scripts/00a_match_cik_automatic.R")
```
- **Input**: `data/raw/GridExport_*.xlsx` (export SDC) + `data/raw/companies.rds`
- **Output**: `data/interim/deals_cik_auto_matched.xlsx`
- **Cosa fa**: 
  - Match automatico tra target name e database SEC companies
  - Genera lista di target senza match per review manuale

#### Step 0b: Review manuale (IN EXCEL)
1. Apri `data/interim/deals_cik_auto_matched.xlsx`
2. Correggi i CIK errati
3. Aggiungi CIK per i target non matchati (cerca su SEC EDGAR)
4. **Salva come** `data/interim/deals_with_cik_matches.xlsx`

> ⚠️ **IMPORTANTE**: Il file `deals_with_cik_matches.xlsx` contiene le tue correzioni manuali. 
> Non sovrascriverlo a meno che tu non voglia rifare le correzioni!

---

### PIPELINE PRINCIPALE

#### Step 1: Pulizia dati
```r
source("scripts/01_ingest_clean_deals.R")
```
- **Input**: `data/interim/deals_with_cik_matches.xlsx` (con CIK corretti)
- **Output**: `data/interim/deals_clean.rds`

#### Step 2: Costruzione indice EDGAR
```r
source("scripts/02_ingest_build_edgar_index.R")
```
- **Input**: `data/interim/deals_clean.rds`
- **Output**: `data/interim/edgar_10k_index.rds`

#### Step 3: Match Deal → Filing
```r
source("scripts/03_merge_match_deals_to_filings.R")
```
- **Input**: `deals_clean.rds` + `edgar_10k_index.rds`
- **Output**: `data/interim/deals_filing_matched.rds` (UNA riga per deal)

#### Step 4: Preparazione manifest
```r
source("scripts/04_ingest_download_10k_filings.R")
```
- **Input**: `deals_filing_matched.rds`
- **Output**: `data/interim/filings_manifest_sec_api.rds`

#### Step 5: Estrazione testo (SEC-API) ⚠️ COSTOSO
```r
source("scripts/05_nlp_parse_sections.R")
```
- **Input**: manifest
- **Output**: `data/interim/parsed_sections.rds` (UNA riga per filing)
- **Nota**: Usa crediti SEC-API. Ha cache, quindi re-run sono economici.

#### Step 6: Merge finale
```r
source("scripts/06_merge_final_dataset.R")
```
- **Input**: `deals_filing_matched.rds` + `parsed_sections.rds`
- **Output**: `data/processed/deals_with_10k_text.rds`

---

## Scenari Comuni

### Scenario A: Prima volta (nuovo progetto)
```r
# 1. Match CIK automatico
source("scripts/00a_match_cik_automatic.R")

# 2. [MANUALE] Apri Excel, correggi CIK, salva come deals_with_cik_matches.xlsx

# 3. Pipeline completa
source("scripts/01_ingest_clean_deals.R")
source("scripts/02_ingest_build_edgar_index.R")
source("scripts/03_merge_match_deals_to_filings.R")
source("scripts/04_ingest_download_10k_filings.R")
source("scripts/05_nlp_parse_sections.R")
source("scripts/06_merge_final_dataset.R")
```

### Scenario B: Fix dati esistenti (dopo errore duplicati)
```r
# Fix una tantum
source("scripts/00_fix_existing_data.R")

# Poi merge finale
source("scripts/06_merge_final_dataset.R")
```

### Scenario C: Aggiungere nuovi deal al campione
```r
# 1. Aggiorna SDC export
# 2. Ri-esegui match CIK (ma NON sovrascrivere le correzioni manuali precedenti!)
# 3. Aggiungi solo i nuovi deal a deals_with_cik_matches.xlsx
# 4. Ri-esegui pipeline da Step 1
```

### Scenario D: Cambiare parametri (es. lag minimo)
```r
# Modifica config/pipeline_config.R
# Ri-esegui da Step 3 (non serve ri-scaricare filing)
source("scripts/03_merge_match_deals_to_filings.R")
source("scripts/04_ingest_download_10k_filings.R")
# Step 5 userà la cache, quindi sarà veloce
source("scripts/05_nlp_parse_sections.R")
source("scripts/06_merge_final_dataset.R")
```

---

## File Chiave

| File | Contiene | Creato da |
|------|----------|-----------|
| `data/raw/companies.rds` | Database SEC companies per CIK lookup | Download SEC |
| `data/interim/deals_with_cik_matches.xlsx` | **Deal + CIK (con correzioni manuali)** | Tu (manuale) |
| `data/interim/deals_clean.rds` | Deal puliti, pronti per matching | Step 1 |
| `data/interim/edgar_10k_index.rds` | Indice di tutti i 10-K | Step 2 |
| `data/interim/deals_filing_matched.rds` | 1 riga per deal, con filing info | Step 3 |
| `data/interim/parsed_sections.rds` | 1 riga per filing, con testo | Step 5 |
| `data/processed/deals_with_10k_text.rds` | Dataset finale | Step 6 |

---

## Troubleshooting

### "parsed_sections has duplicates"
```r
source("scripts/00_fix_existing_data.R")
```

### Target senza match CIK
1. Cerca su SEC EDGAR: https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany
2. Aggiungi CIK manualmente in `deals_with_cik_matches.xlsx`
3. Ri-esegui pipeline da Step 1

### Filing non trovato per un deal
Cause possibili:
- CIK errato → correggi in Excel
- Target non era public (no 10-K) → rimuovi dal campione
- IPO troppo recente → nessun 10-K disponibile prima dell'annuncio
