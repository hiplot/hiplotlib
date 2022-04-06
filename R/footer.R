new_task_step(
  "done", "en:Sessioninfo (R);zh_cn:运行环境 (R)",
  function() {
    flog.info(sprintf("Task done: %s/%s.", opt$module, opt$tool))
    print_sessioninfo()
  }
)
