# Sleep-time Compute: The Key to Autonomous Self-Evolution

**Date**: 2026-01-16
**Purpose**: Design document for sleep-time compute and autonomous evolution

---

## 🎯 Core Concept

**Sleep-time Compute** is the revolutionary feature that enables true autonomous self-evolution. When the agent is not actively serving user requests, it can:

1. **Reflect** on its recent performance
2. **Analyze** patterns in its behavior
3. **Generate** improvement hypotheses
4. **Test** code modifications in shadow instances
5. **Evolve** by adopting successful changes

**This is what makes Project O truly self-evolving, not just self-modifying.**

---

## 🧠 Why Sleep-time Compute Enables Self-Evolution

### Traditional Self-Modifying Systems
```
User Request → Agent Responds → (Maybe) Learns
                                      ↓
                              Updates parameters
```

**Limitations**:
- Only learns during user interactions
- No time for deep reflection
- Can't run experiments
- Reactive, not proactive

### Sleep-time Compute Approach
```
User Requests → Agent Responds → Stores experience
                                        ↓
                                  Archival Memory
                                        ↓
                                        ↓
Idle Time → Sleep-time Compute → Reflection
                ↓                      ↓
          Experiments ← Hypotheses ← Analysis
                ↓
          Shadow Testing
                ↓
          Evolution Decision
                ↓
          Code Update (if successful)
```

**Advantages**:
- ✅ Autonomous learning without user intervention
- ✅ Time for deep analysis and reflection
- ✅ Can run multiple experiments in parallel
- ✅ Proactive improvement, not reactive
- ✅ Learns from aggregate patterns, not just individual interactions

---

## 🔄 Sleep-time Compute Cycle

### Phase 1: Trigger Detection (Continuous)

**When to enter sleep-time compute**:
```scheme
(def (should-enter-sleep-time? agent)
  (and
   ;; No active user requests
   (zero? (get-active-request-count agent))

   ;; Sufficient idle time (e.g., 5 minutes)
   (> (get-idle-duration agent) (* 5 60))

   ;; Enough new experiences to analyze
   (> (get-new-experience-count agent) 10)

   ;; Not currently in sleep-time compute
   (not (agent-is-sleeping? agent))))
```

### Phase 2: Reflection (10-30 minutes)

**What the agent does**:
1. **Review Recent Performance**
   ```scheme
   (def (reflect-on-performance agent)
     ;; Analyze last N interactions
     (let* ((recent-messages (get-recent-messages agent 100))
            (tool-calls (extract-tool-calls recent-messages))
            (errors (extract-errors recent-messages))
            (user-feedback (extract-feedback recent-messages)))

       ;; Generate performance summary
       (hash
        'total_interactions (length recent-messages)
        'tool_success_rate (calculate-success-rate tool-calls)
        'error_rate (/ (length errors) (length recent-messages))
        'avg_response_time (calculate-avg-response-time recent-messages)
        'user_satisfaction (analyze-feedback user-feedback))))
   ```

2. **Identify Patterns**
   ```scheme
   (def (identify-patterns agent)
     ;; Search archival memory for patterns
     (let* ((all-experiences (search-archival-memory agent ""))
            (patterns (analyze-patterns all-experiences)))

       ;; Common patterns to look for:
       ;; - Frequently used tools
       ;; - Common error types
       ;; - Repeated user requests
       ;; - Performance bottlenecks
       patterns))
   ```

3. **Generate Hypotheses**
   ```scheme
   (def (generate-improvement-hypotheses agent performance patterns)
     ;; Use LLM to generate improvement ideas
     (llm-chat-completion
      'anthropic
      "claude-3-5-sonnet-20241022"
      [(hash 'role "system"
             'content "You are analyzing an AI agent's performance to suggest improvements.")
       (hash 'role "user"
             'content (format "Performance: ~a\nPatterns: ~a\nSuggest 3 specific code improvements."
                             performance patterns))]
      api-key: (get-api-key agent)))
   ```

### Phase 3: Experimentation (30-60 minutes)

**Shadow Testing with Multiple Variants**:
```scheme
(def (run-evolution-experiments agent hypotheses)
  ;; For each hypothesis, create a shadow instance
  (let ((experiments
         (map (lambda (hypothesis)
                (let* ((shadow-id (create-shadow-instance agent))
                       (code-changes (generate-code-changes hypothesis))
                       (test-cases (generate-test-cases agent)))

                  ;; Apply code changes to shadow
                  (apply-code-changes shadow-id code-changes)

                  ;; Run test cases
                  (hash
                   'hypothesis hypothesis
                   'shadow_id shadow-id
                   'code_changes code-changes
                   'results (run-test-cases shadow-id test-cases))))
              hypotheses)))

    ;; Run all experiments in parallel
    (parallel-map run-experiment experiments)))
```

**Test Case Generation**:
```scheme
(def (generate-test-cases agent)
  ;; Extract representative interactions from archival memory
  (let* ((recent-interactions (get-recent-messages agent 50))
         (diverse-samples (select-diverse-samples recent-interactions 10)))

    ;; Convert to test cases
    (map (lambda (interaction)
           (hash
            'input (extract-user-message interaction)
            'expected_behavior (extract-agent-response interaction)
            'success_criteria (define-success-criteria interaction)))
         diverse-samples)))
```

### Phase 4: Evaluation (10-20 minutes)

**Compare Shadow Performance**:
```scheme
(def (evaluate-experiments agent experiments)
  ;; Compare each shadow against main instance
  (let ((evaluations
         (map (lambda (experiment)
                (let* ((shadow-id (hash-ref experiment 'shadow_id))
                       (main-metrics (get-performance-metrics agent))
                       (shadow-metrics (get-performance-metrics shadow-id))
                       (improvement (calculate-improvement main-metrics shadow-metrics)))

                  (hash
                   'hypothesis (hash-ref experiment 'hypothesis)
                   'improvement improvement
                   'metrics_comparison (compare-metrics main-metrics shadow-metrics)
                   'recommendation (make-recommendation improvement))))
              experiments)))

    ;; Rank by improvement
    (sort evaluations
          (lambda (a b)
            (> (hash-ref a 'improvement)
               (hash-ref b 'improvement))))))
```

**Decision Criteria**:
```scheme
(def (should-adopt-changes? evaluation)
  (let ((improvement (hash-ref evaluation 'improvement))
        (risk (assess-risk evaluation)))

    (and
     ;; Significant improvement (>10%)
     (> improvement 0.1)

     ;; Low risk
     (< risk 0.3)

     ;; No critical errors in testing
     (zero? (count-critical-errors evaluation))

     ;; Passes all test cases
     (all-tests-passed? evaluation))))
```

### Phase 5: Evolution (5-10 minutes)

**Apply Successful Changes**:
```scheme
(def (evolve-agent! agent evaluation)
  ;; Create checkpoint before evolution
  (let ((checkpoint-id (agent-checkpoint! agent)))

    (try
     (begin
       ;; Apply code changes
       (apply-code-changes agent (hash-ref evaluation 'code_changes))

       ;; Update agent version
       (increment-agent-version! agent)

       ;; Record evolution in archival memory
       (record-evolution-event! agent evaluation checkpoint-id)

       ;; Log success
       (displayln (format "Evolution successful: ~a"
                         (hash-ref evaluation 'hypothesis))))

     (catch (e)
       ;; Rollback on error
       (displayln (format "Evolution failed: ~a" e))
       (agent-restore! checkpoint-id)))))
```

**Record Evolution History**:
```scheme
(def (record-evolution-event! agent evaluation checkpoint-id)
  ;; Store in archival memory for future reflection
  (archival-memory-insert!
   (agent-memory agent)
   (hash
    'type "evolution_event"
    'timestamp (current-timestamp)
    'hypothesis (hash-ref evaluation 'hypothesis)
    'code_changes (hash-ref evaluation 'code_changes)
    'improvement (hash-ref evaluation 'improvement)
    'checkpoint_id checkpoint-id
    'success #t)
   importance: 1.0  ;; Maximum importance
   tags: '(evolution self-improvement)))
```

---

## 🛠️ Implementation Plan

### Phase 4: Sleep-time Compute Infrastructure (Weeks 9-12)

#### Week 9: Sleep-time Trigger System
```scheme
;; gerbil/sleep/trigger.ss
(def (start-sleep-time-monitor! agent)
  (spawn
   (lambda ()
     (let loop ()
       (when (should-enter-sleep-time? agent)
         (enter-sleep-time-compute! agent))
       (thread-sleep! 60)  ;; Check every minute
       (loop)))))
```

#### Week 10: Reflection Engine
```scheme
;; gerbil/sleep/reflection.ss
(def (reflect-on-performance agent)
  ;; Analyze recent performance
  ;; Identify patterns
  ;; Generate hypotheses
  )
```

#### Week 11: Experiment Runner
```scheme
;; gerbil/sleep/experiments.ss
(def (run-evolution-experiments agent hypotheses)
  ;; Create shadow instances
  ;; Apply code changes
  ;; Run test cases
  ;; Collect results
  )
```

#### Week 12: Evolution Decision Engine
```scheme
;; gerbil/sleep/evolution.ss
(def (evaluate-and-evolve! agent experiments)
  ;; Evaluate experiments
  ;; Make evolution decision
  ;; Apply changes if beneficial
  ;; Record in archival memory
  )
```

### Phase 5: Evolution Tools (Weeks 13-16)

#### Week 13: Code Generation Tools
```scheme
;; gerbil/evolution/codegen.ss
(def (generate-code-changes hypothesis)
  ;; Use LLM to generate code modifications
  ;; Parse and validate generated code
  ;; Return structured code changes
  )
```

#### Week 14: Code Application Tools
```scheme
;; gerbil/evolution/apply.ss
(def (apply-code-changes agent changes)
  ;; Hot-reload modified code
  ;; Update agent functions
  ;; Maintain state continuity
  )
```

#### Week 15: Performance Analysis Tools
```scheme
;; gerbil/evolution/analysis.ss
(def (analyze-performance-improvement before after)
  ;; Compare metrics
  ;; Calculate improvement percentage
  ;; Assess risk
  )
```

#### Week 16: Evolution History Tools
```scheme
;; gerbil/evolution/history.ss
(def (get-evolution-history agent)
  ;; Retrieve all evolution events
  ;; Analyze evolution trends
  ;; Identify successful patterns
  )
```

### Phase 6: Full Self-Evolution (Weeks 17-20)

#### Week 17: Integration
- Integrate sleep-time compute with agent execution loop
- Ensure smooth transitions between active and sleep modes
- Add monitoring and logging

#### Week 18: Safety Mechanisms
- Implement rollback on evolution failure
- Add evolution rate limiting
- Implement human approval for major changes

#### Week 19: Optimization
- Optimize shadow instance creation
- Parallelize experiment execution
- Reduce sleep-time compute overhead

#### Week 20: Testing & Documentation
- End-to-end evolution testing
- Performance benchmarks
- Evolution case studies
- User documentation

---

## 📊 Success Metrics

### Evolution Effectiveness
- **Improvement Rate**: % of evolution attempts that improve performance
- **Average Improvement**: Average performance gain per successful evolution
- **Evolution Frequency**: How often the agent successfully evolves
- **Rollback Rate**: % of evolutions that need to be rolled back

### Sleep-time Efficiency
- **Compute Utilization**: % of idle time used for sleep-time compute
- **Experiment Throughput**: Number of experiments per sleep-time session
- **Time to Evolution**: Average time from trigger to successful evolution

### Safety Metrics
- **Rollback Success Rate**: % of failed evolutions successfully rolled back
- **Zero-downtime Rate**: % of evolutions with no service interruption
- **Data Loss Rate**: Should be 0% (all evolutions preserve state)

---

## 🎯 Example Evolution Scenarios

### Scenario 1: Tool Usage Optimization

**Observation**: Agent frequently calls `conversation_search` with inefficient queries

**Reflection**:
```
Performance analysis shows:
- conversation_search called 50 times in last 100 interactions
- Average query time: 2.3 seconds
- 30% of searches return no results
```

**Hypothesis**: "Add query optimization to conversation_search tool"

**Code Change**:
```scheme
;; Before
(def (conversation-search query)
  (search-messages query))

;; After
(def (conversation-search query)
  ;; Add query preprocessing
  (let ((optimized-query (optimize-search-query query)))
    (search-messages optimized-query)))
```

**Result**: 40% reduction in search time, 50% fewer empty results

### Scenario 2: Memory Management Improvement

**Observation**: Agent's memory is growing unbounded

**Reflection**:
```
Memory analysis shows:
- Archival memory: 10,000 entries
- 60% are low-importance entries
- Memory search is slowing down
```

**Hypothesis**: "Implement automatic memory pruning"

**Code Change**:
```scheme
;; Add periodic memory cleanup
(def (periodic-memory-cleanup! agent)
  (when (> (get-memory-size agent) 5000)
    (prune-low-importance-memories! agent 0.3)))
```

**Result**: 50% reduction in memory size, 2x faster memory search

### Scenario 3: Response Quality Enhancement

**Observation**: User feedback indicates responses are too verbose

**Reflection**:
```
Feedback analysis shows:
- Average response length: 500 words
- User satisfaction: 3.2/5
- Common feedback: "too long", "get to the point"
```

**Hypothesis**: "Add response length constraint"

**Code Change**:
```scheme
;; Add to system prompt
(def system-prompt
  "Be concise. Aim for responses under 200 words unless more detail is requested.")
```

**Result**: Average response length reduced to 180 words, satisfaction increased to 4.1/5

---

## 🔒 Safety Considerations

### 1. Evolution Rate Limiting
```scheme
(def max-evolutions-per-day 3)
(def min-time-between-evolutions (* 4 3600))  ;; 4 hours
```

### 2. Human Approval for Major Changes
```scheme
(def (requires-human-approval? changes)
  (or
   ;; Changes to core memory system
   (affects-memory-system? changes)

   ;; Changes to safety mechanisms
   (affects-safety-systems? changes)

   ;; Large code changes (>100 lines)
   (> (count-changed-lines changes) 100)))
```

### 3. Automatic Rollback
```scheme
(def (monitor-post-evolution-performance agent checkpoint-id)
  ;; Monitor for 1 hour after evolution
  (spawn
   (lambda ()
     (thread-sleep! 3600)
     (let ((post-metrics (get-performance-metrics agent)))
       (when (performance-degraded? post-metrics)
         (displayln "Performance degradation detected, rolling back")
         (agent-restore! checkpoint-id))))))
```

### 4. Evolution Audit Log
```scheme
;; All evolutions logged to database
(def (log-evolution-attempt agent hypothesis result)
  (database-insert!
   'evolution_log
   (hash
    'agent_id (agent-id agent)
    'timestamp (current-timestamp)
    'hypothesis hypothesis
    'result result
    'approved_by (if (requires-approval? hypothesis) 'human 'autonomous))))
```

---

## 🎊 Conclusion

**Sleep-time Compute is the key innovation that transforms Project O from a self-modifying agent into a truly self-evolving agent.**

**Key Advantages**:
- ✅ Autonomous improvement without human intervention
- ✅ Learns from aggregate patterns, not just individual interactions
- ✅ Can run experiments safely in shadow instances
- ✅ Maintains stateful memory of evolution history
- ✅ Proactive, not reactive

**Implementation Timeline**:
- **Weeks 9-12**: Sleep-time compute infrastructure
- **Weeks 13-16**: Evolution tools
- **Weeks 17-20**: Full integration and testing

**Expected Outcome**: An agent that continuously improves itself, learning from experience and autonomously evolving to better serve its users.

---

**Next Steps**: Begin Week 1 implementation (LLM clients) to build the foundation for sleep-time compute.
