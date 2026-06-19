# Arkansas INBRE Voucher Tracker Dashboard

This is a simple web tool to help you see and understand our Arkansas INBRE voucher data without having to look at messy spreadsheets or complicated code. 

You can use the live web version right here: [https://kalyanidhusia.shinyapps.io/arinbre-tracker/](https://kalyanidhusia.shinyapps.io/arinbre-tracker/)

---

##  What is it?
This tool is a visual dashboard. It takes two different files exported from REDCap (the **Voucher Applications file** and the **Voucher Reports file**), cleans up all the typos and messy dates automatically behind the scenes, and connects them together into clean charts and searchable lists.

## Who is it for?
* **Lab Members & PIs:** To quickly check project statuses and look up what results were produced.
* **University Administrators:** To see which schools are using which core facilities the most.
* **Grant Writers:** To find exact statistics, project counts, and publication citations for NIH progress reports.

---

## 🗂️ What are the Tabs and Tables?

### 1. Executive Summary (The Dashboard Tab)
This tab shows you the big picture using simple charts and summary cards:
* **Top Metric Cards:** Instantly see the total number of vouchers awarded, how many reports have been turned in, and the count of presentations, manuscripts, and grants achieved.
* **Funding Cycle Graph:** A bar chart showing the growth of deliverables over the years.
* **ARINBRE Home Institution Graph:** A chart showing exactly how many vouchers each university received (with messy names like "UALR" and "UA Little Rock" cleaned and combined into one item).
* **Voucher Classification Graph:** A breakdown showing how many Student Vouchers, Research Vouchers, or Curriculum Development awards were given out.
* **Core Facility Charts:** Clean, color-coded graphs ranking which core facilities (like Proteomics or DNA Sequencing) are used the most, plus a cross-tab map showing which university uses which core facility.

### 2. Scientific Impact Data (The Information Tab)
This tab contains a single, massive full-page spreadsheet table. It lists:
* Every investigator's name, school, voucher type, and requested budget.
* Quick indicators (⭐) showing if they produced a presentation, a manuscript, or a grant proposal.
* Read-only notes detailing the exact **Scientific Outcomes, Student Mentorship Tracker, and Manuscript Citations / DOIs**.
* **How to use this table:** You can type any keyword (like an author's name, a school, or a term like "cancer") into the search bars to instantly find matching text for your reports.

### 3. Linkage Audit & Record Matcher (The Quality Tab)
Because the application file and report file do not share a single common ID code, this tab uses a smart background scoring model to check how well the rows match by comparing text similarity, names, and timeline proximities:
* **Green Rows:** High-confidence matches where names and titles line up perfectly.
* **Yellow Rows:** Review items where a name variation or an altered project title means you should take a quick look to verify the link.

---

## How to Use It

### Option A: Use the Live Web Link (Easiest)
1. Go to the live website: [https://kalyanidhusia.shinyapps.io/arinbre-tracker/](https://kalyanidhusia.shinyapps.io/arinbre-tracker/)
2. On the left sidebar panel, click **Browse...** to upload your `PID_1239.csv` (Applications) file.
3. Click the second upload button to drop in your `PID_1242.csv` (Reports) file.
4. The dashboard will instantly update!

### Option B: Run it on Your Own Computer (For Local Use)
1. Clone or download this repository folder.
2. Inside the root folder, create a new subfolder named `raw_data`.
3. Save your REDCap CSV files inside that folder as `applications.csv` and `reports.csv`. 
4. *(Note: The `raw_data/` folder is explicitly listed in our hidden `.gitignore` file, meaning your private university data will never accidentally get uploaded to the public internet if you push code updates).*
5. Open `app.R` in RStudio and click **Run App** in the top right corner!


## Contact

for any queries or suggestion, please reach out to [KDhusia@uams.edu](KDhusia@uams.edu)
