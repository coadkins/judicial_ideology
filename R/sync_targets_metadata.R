targets::tar_option_set(
  repository_meta = "aws",
  resources = targets::tar_resources(
    targets::tar_resources_aws(
      bucket = Sys.getenv("S3_BUCKET"),
      prefix = "mvn_cases",
      endpoint = Sys.getenv("S3_ENDPOINT"),
      region = Sys.getenv("S3_REGION")
    )
  )
)

targets::tar_meta_sync()
