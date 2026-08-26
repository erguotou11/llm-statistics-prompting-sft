
select
        model_name
        ,prompt_strategy
        ,avg_weighted_score
        ,avg_correct_score
        ,avg_reasoning_score
        ,avg_logical_score
        ,avg_explanation_score
        ,avg_statistics_interpretation_score
from
        (select
                model_name
                ,prompt_strategy
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy) as concat_prompt_strategy
                ,round(avg(weighted_score),2) as avg_weighted_score
                ,round(avg(correct_score),2) as avg_correct_score
                ,round(avg(reasoning_score),2) as avg_reasoning_score
                ,round(avg(logical_score),2) as avg_logical_score
                ,round(avg(explanation_score),2) as avg_explanation_score
                ,round(avg(statistics_interpretation_score),2) as avg_statistics_interpretation_score
        from    eval
        where   model_name in ('Llama3-8B','Qwen2.5-7B','DeepSeek-R1-7B')
        group by model_name
                ,prompt_strategy
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy)
        )m
order by model_name
        ,concat_prompt_strategy;



select
        model_name
        ,prompt_strategy
        ,question_type
        ,avg_weighted_score
        ,avg_correct_score
        ,avg_reasoning_score
        ,avg_logical_score
        ,avg_explanation_score
        ,avg_statistics_interpretation_score
from
        (select
                model_name
                ,prompt_strategy
                ,question_type
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy) as concat_prompt_strategy
                ,round(avg(weighted_score),2) as avg_weighted_score
                ,round(avg(correct_score),2) as avg_correct_score
                ,round(avg(reasoning_score),2) as avg_reasoning_score
                ,round(avg(logical_score),2) as avg_logical_score
                ,round(avg(explanation_score),2) as avg_explanation_score
                ,round(avg(statistics_interpretation_score),2) as avg_statistics_interpretation_score
        from    eval
        where   model_name in ('Llama3-8B','Qwen2.5-7B','DeepSeek-R1-7B')
        group by model_name
                ,prompt_strategy
               ,question_type
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy)
        )m
order by model_name
        ,concat_prompt_strategy
        ,question_type;





        select
                model_name
                ,question_type
                ,turns
                ,count(*) as cnt
                ,round(avg(weighted_score),2) as avg_weighted_score
                ,round(avg(correct_score),2) as avg_correct_score
                ,round(avg(reasoning_score),2) as avg_reasoning_score
        from    eval
        where   model_name in ('Llama3-8B','Qwen2.5-7B','DeepSeek-R1-7B')
        and     prompt_strategy='Student-Tutor'
        group by model_name
                ,question_type
                ,turns
        order by model_name
        ,question_type
        ,turns;

        select
                model_name
                ,hard_level
                ,turns
                ,count(*) as cnt
                ,round(avg(weighted_score),2) as avg_weighted_score
                ,round(avg(correct_score),2) as avg_correct_score
                ,round(avg(reasoning_score),2) as avg_reasoning_score
        from    eval
        where   model_name in ('Llama3-8B','Qwen2.5-7B','DeepSeek-R1-7B')
        and     prompt_strategy='Student-Tutor'
        group by model_name
                ,hard_level
                ,turns
        order by model_name
                ,hard_level
                ,turns;


SELECT
    model_name,
    turns,
    COUNT(*) AS cnt,
    ROUND(AVG(weighted_score), 2) AS avg_weighted_score,
    ROUND(AVG(correct_score), 2) AS avg_correct_score,
    ROUND(AVG(reasoning_score), 2) AS avg_reasoning_score
FROM eval
WHERE model_name IN (
    'Llama3-8B',
    'Qwen2.5-7B',
    'DeepSeek-R1-7B'
)
AND prompt_strategy = 'Student-Tutor'
GROUP BY
    model_name,
    turns
ORDER BY
    model_name,
    turns;




SELECT
    model_name,
    hard_level,
    question_type,
    knowledge,
    turns,
    COUNT(*) AS cnt
FROM eval
WHERE model_name IN (
    'Llama3-8B',
    'Qwen2.5-7B',
    'DeepSeek-R1-7B'
)
AND prompt_strategy = 'Student-Tutor'
GROUP BY  model_name,
    hard_level,
    question_type,
    knowledge,
    turns






select
        model_name
        ,prompt_strategy
        ,count(*)
from    eval
group by  model_name
        ,prompt_strategy
order by model_name
        ,prompt_strategy

alter table eval replace

UPDATE eval
SET prompt_strategy = 'CoT'
WHERE prompt_strategy = 'Chain-of-Thought';


select
model_name
,prompt_strategy
,error_type
,count(*)
from eval
WHERE model_name IN (
    'Llama3-8B',
    'Qwen2.5-7B',
    'DeepSeek-R1-7B'
)
group by model_name
,prompt_strategy
,error_type
order by model_name
,prompt_strategy
,error_type



select
error_type
,count(*) c
from eval
WHERE model_name IN (
    'Llama3-8B',
    'Qwen2.5-7B',
    'DeepSeek-R1-7B'
)
group by error_type
order by c desc



select
        model_name
        ,prompt_strategy
        ,avg_weighted_score
        ,avg_correct_score
        ,avg_reasoning_score
        ,avg_logical_score
        ,avg_explanation_score
        ,avg_statistics_interpretation_score
from
        (select
                model_name
                ,prompt_strategy
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy) as concat_prompt_strategy
                ,round(avg(weighted_score),2) as avg_weighted_score
                ,round(avg(correct_score),2) as avg_correct_score
                ,round(avg(reasoning_score),2) as avg_reasoning_score
                ,round(avg(logical_score),2) as avg_logical_score
                ,round(avg(explanation_score),2) as avg_explanation_score
                ,round(avg(statistics_interpretation_score),2) as avg_statistics_interpretation_score
        from    eval_sft
        where   model_name in ('Llama3-8B','Qwen2.5-7B','DeepSeek-R1-7B')
        group by model_name
                ,prompt_strategy
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy)
        )m
order by model_name
        ,concat_prompt_strategy;





select
        model_name
        ,prompt_strategy
        ,hard_level
        ,avg_weighted_score
        ,avg_correct_score
        ,avg_reasoning_score
        ,avg_logical_score
        ,avg_explanation_score
        ,avg_statistics_interpretation_score
from
        (select
                model_name
                ,prompt_strategy
                ,hard_level
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy) as concat_prompt_strategy
                ,round(avg(weighted_score),2) as avg_weighted_score
                ,round(avg(correct_score),2) as avg_correct_score
                ,round(avg(reasoning_score),2) as avg_reasoning_score
                ,round(avg(logical_score),2) as avg_logical_score
                ,round(avg(explanation_score),2) as avg_explanation_score
                ,round(avg(statistics_interpretation_score),2) as avg_statistics_interpretation_score
        from    eval
        where   model_name in ('Llama3-8B','Qwen2.5-7B','DeepSeek-R1-7B')
        group by model_name
                ,prompt_strategy
                ,hard_level
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy)
        )m
order by model_name
        ,concat_prompt_strategy
        ,hard_level;





select
        model_name
        ,prompt_strategy
        ,knowledge
        ,avg_weighted_score
        ,avg_correct_score
        ,avg_reasoning_score
        ,avg_logical_score
        ,avg_explanation_score
        ,avg_statistics_interpretation_score
from
        (select
                model_name
                ,prompt_strategy
                ,knowledge
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy) as concat_prompt_strategy
                ,round(avg(weighted_score),2) as avg_weighted_score
                ,round(avg(correct_score),2) as avg_correct_score
                ,round(avg(reasoning_score),2) as avg_reasoning_score
                ,round(avg(logical_score),2) as avg_logical_score
                ,round(avg(explanation_score),2) as avg_explanation_score
                ,round(avg(statistics_interpretation_score),2) as avg_statistics_interpretation_score
        from    eval
        where   model_name in ('Llama3-8B','Qwen2.5-7B','DeepSeek-R1-7B')
        group by model_name
                ,prompt_strategy
                ,knowledge
                ,concat(if(prompt_strategy='Direct','A',''),prompt_strategy)
        )m
order by model_name
        ,concat_prompt_strategy
        ,knowledge;

select
    knowledge
    ,length(knowledge)
from    eval
        where   model_name in ('Llama3-8B','Qwen2.5-7B','DeepSeek-R1-7B')
group by knowledge
,length(knowledge)