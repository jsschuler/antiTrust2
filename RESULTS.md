# Results log

## 2026-11-03: by-privacy-level analysis of the October sweeps

Data: the four October 2026 sweeps (`baseline`, `vpn`, `deletion`, `sharing`),
500 runs each, 250 at `privacyVal` 0.5 and 250 at 1.0. Numbers come from
`dataAnalysisPVal.R` (`effect_size_summary.csv`, 18 rows).

### Headline

Privacy remedies that let users stay on the incumbent strengthen it. Only data
sharing erodes its dominance, and it does so at the expense of the most
privacy-minded users.

Incumbent ("Hoogle") share of searches from tick 50 on:

| | Less privacy-minded (`privacyVal` 1.0) | More privacy-minded (`privacyVal` 0.5) |
| --- | --- | --- |
| Baseline (entrant only) | 80.7% | 61.4% |
| VPN | +0.6 points (p = 0.028) | +5.7 points (p < 0.002) |
| Deletion right | +0.8 points (p = 0.014) | +1.5 points (p = 0.002) |
| Sharing right | −26.0 points (p < 0.002) | −12.1 points (p < 0.002) |

Every intervention is also significant at the agent level and over time, at
both privacy levels (p ≤ 0.011).

### Findings

1. **VPN and deletion help the incumbent.** Both let privacy-minded users
   protect themselves without leaving it. With VPN, the most privacy-minded
   agents stay on the incumbent about 30% of the time, against about 12%
   without, and in more privacy-minded runs the median incumbent share settles
   around 68% against 61%, a gap that persists to tick 500. Deletion works the
   same way, more weakly.
2. **Sharing is the only remedy that cuts the incumbent's share.** Copying a
   user's history to the entrant removes the incumbent's personalization
   advantage. Privacy-indifferent agents end up splitting about 50/50 between
   the engines instead of staying about 93% on the incumbent.
3. **Sharing hurts the most privacy-minded users.** Once the entrant holds
   their history it personalizes for them too, so they lose their only
   unpersonalized option, and some drift back to the incumbent (about 48%
   against 30% at a privacy index of 0.9).
4. **Agents respond to their own preference, not the population.** At a given
   privacy index the agent-level curves are nearly identical across the two
   `privacyVal` levels. The population mix only sets how many agents sit at
   each point, which is what moves the aggregate result.

### The sharing result depends on the population

The paper's run (May 2025) used `privacyVal` 0.1, where about 83% of agents
have a privacy index above 0.85 (against 39% at 0.5 and 15% at 1.0). There,
sharing *raised* the incumbent's share, from 28.7% to 46.0%, because the
privacy-minded agents pushed back to the incumbent dominated. The new runs sit
further along the same curve, where the privacy-indifferent majority
dominates.

The model also changed between the two runs (commit `212634e`): sharing now
transfers a fixed snapshot rather than a live link to the incumbent's data, the
incumbent no longer records each search twice, and searches keep both bounds
across failed guesses. How much these contribute to the flip is untested.

### Caveats

- **Sharing adoption is essentially random.** Agents keep the sharing action
  about 49% of the time at every privacy level (43% at the very top), because
  at the moment of choosing both engines personalize identically. Agents can't
  foresee that sharing removes the unpersonalized alternative; this is a
  modelling choice the paper needs to state and defend.
- **Copied history persists by design.** Removing it is the deletion action's
  job, but each experiment enables a single intervention, so in the `sharing`
  experiment the copy is permanent. Sharing combined with deletion is untested.
- **VPN and deletion effects are small** in the less privacy-minded population
  (under 1 point); their substantive effect is in the more privacy-minded runs.

### Open checks

- Run `baseline` and `sharing` at `privacyVal` 0.1 on the current code, to
  confirm the population, not the code fixes, explains the flip.
- Decide whether the paper keeps the sharing scenario, and if so how it
  presents the adoption mechanism.

### Analysis changes behind these numbers

- All three analyses (run, agent, time) are run separately for each
  `privacyVal`. Pooling the two levels put the time-trend median between two
  clusters of runs and left those tests with almost no power.
- The agent-level moving window is ±0.025 on the privacy index (was ±0.0025),
  so curves span 0.025–0.975.
- Time-trend gaps are summed over ticks rather than Monte Carlo estimated.
