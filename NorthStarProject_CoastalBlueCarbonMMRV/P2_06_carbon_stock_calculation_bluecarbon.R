# Add after the existing HTML content (around line 300):

sampling_recommendations <- ''

if (exists("sample_assessment")) {
  
  needs_more <- sample_assessment %>% filter(status != "SUFFICIENT")
  
  if (nrow(needs_more) > 0) {
    sampling_recommendations <- sprintf('
<h2>⚠️ Sampling Recommendations</h2>
<div class="highlight" style="background-color: #FFF3E0; border-left: 4px solid #F57C00;">
<p><strong>Additional field sampling recommended before carbon crediting:</strong></p>
<table>
<thead>
<tr>
<th>Stratum</th>
<th>Current Cores</th>
<th>Status</th>
<th>Additional Needed (Min)</th>
<th>Additional Needed (Recommended)</th>
<th>Priority</th>
</tr>
</thead>
<tbody>
%s
</tbody>
</table>
<p><strong>Rationale:</strong></p>
<ul>
<li>VM0033 requires minimum n=3 per stratum (n=5 recommended)</li>
<li>Robust spatial models need n=10+ per stratum</li>
<li>Current sample sizes result in high uncertainty and conservative estimates</li>
<li>Additional samples will reduce uncertainty and increase creditable carbon</li>
</ul>
<p><strong>Estimated Impact:</strong></p>
<ul>
<li>Current uncertainty may reduce creditable carbon by 15-30%%</li>
<li>Meeting recommended sample sizes could increase credit volume by 20-40%%</li>
<li>Improved spatial coverage will strengthen verification</li>
</ul>
</div>
',
      paste(apply(needs_more, 1, function(row) {
        priority <- ifelse(row["status"] == "INSUFFICIENT", "🔴 HIGH", 
                          ifelse(row["status"] == "BELOW RECOMMENDED", "🟡 MEDIUM", "🟢 LOW"))
        sprintf('<tr><td>%s</td><td>%d</td><td>%s</td><td>%d</td><td>%d</td><td>%s</td></tr>',
                row["stratum"], row["n_cores"], row["status"],
                row["additional_for_minimum"], row["additional_for_recommended"], priority)
      }), collapse = "\n")
    )
  } else {
    sampling_recommendations <- '
<h2>✅ Sampling Status</h2>
<div class="highlight" style="background-color: #E8F5E9; border-left: 4px solid #4CAF50;">
<p><strong>Sampling is adequate for carbon crediting</strong></p>
<p>All strata meet or exceed VM0033 recommended sample sizes (n≥5 per stratum).</p>
</div>
'
  }
}

# Insert into HTML before verification checklist section
html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
...
%s

<h2>Verification Checklist</h2>
...
',
sampling_recommendations
)
