! HSD Library - Configuration & Schema Example
!
! This example demonstrates how to use HSD for robust configuration file parsing.
! It showcases:
! - Defining a validation schema
! - Enforcing required fields and types
! - Validating value ranges and enumerations
! - Checking for unknown fields (strict validation)
! - Reading values with attributes (units)
!
program config_demo
  use hsd, only: hsd_table, hsd_error_t, hsd_schema_t, dp, &
    & hsd_load, hsd_get, hsd_get_or, hsd_get_attrib, &
    & schema_init, schema_destroy, schema_add_field, schema_add_field_enum, &
    & schema_validate_strict, &
    & FIELD_REQUIRED, FIELD_OPTIONAL, FIELD_TYPE_TABLE, FIELD_TYPE_INTEGER, &
    & FIELD_TYPE_STRING, FIELD_TYPE_REAL, FIELD_TYPE_LOGICAL
  implicit none (type, external)

  type(hsd_table) :: config
  type(hsd_schema_t) :: schema
  type(hsd_error_t), allocatable :: errors(:), io_error

  ! Configuration variables
  integer :: port, max_conn, pool_size
  logical :: logging, beta_ui
  real(dp) :: timeout
  character(len=:), allocatable :: host, db_driver, unit_str

  print '(A)', "--- HSD Configuration Validation Demo ---"
  print *

  ! -------------------------------------------------------------------------
  ! 1. Define the Schema
  ! -------------------------------------------------------------------------
  print '(A)', "1. Defining schema..."
  call schema_init(schema)

  ! Server Section
  call schema_add_field(schema, "Server", FIELD_REQUIRED, FIELD_TYPE_TABLE)
  call schema_add_field(schema, "Server/Port", FIELD_REQUIRED, FIELD_TYPE_INTEGER, &
      & min_int=1024, max_int=65535)
  call schema_add_field(schema, "Server/Host", FIELD_OPTIONAL, FIELD_TYPE_STRING)
  call schema_add_field(schema, "Server/MaxConnections", FIELD_OPTIONAL, FIELD_TYPE_INTEGER, &
      & min_int=1)
  call schema_add_field(schema, "Server/EnableLogging", FIELD_OPTIONAL, FIELD_TYPE_LOGICAL)
  call schema_add_field(schema, "Server/Timeout", FIELD_REQUIRED, FIELD_TYPE_REAL, &
      & min_real=0.0_dp)

  ! Database Section
  call schema_add_field(schema, "Database", FIELD_REQUIRED, FIELD_TYPE_TABLE)
  call schema_add_field_enum(schema, "Database/Driver", FIELD_REQUIRED, &
      & ["postgresql", "mysql     ", "sqlite    "])
  call schema_add_field(schema, "Database/ConnectionString", FIELD_REQUIRED, FIELD_TYPE_STRING)
  call schema_add_field(schema, "Database/PoolSize", FIELD_OPTIONAL, FIELD_TYPE_INTEGER, &
      & min_int=1, max_int=100)

  ! Feature Flags
  call schema_add_field(schema, "FeatureFlags", FIELD_OPTIONAL, FIELD_TYPE_TABLE)
  ! Allow any logical under FeatureFlags (wildcard support could be added to schema later)
  ! For now we explicitly list known flags
  call schema_add_field(schema, "FeatureFlags/BetaUI", FIELD_OPTIONAL, FIELD_TYPE_LOGICAL)
  call schema_add_field(schema, "FeatureFlags/NewSearch", FIELD_OPTIONAL, FIELD_TYPE_LOGICAL)

  ! -------------------------------------------------------------------------
  ! 2. Load and Validate
  ! -------------------------------------------------------------------------
  print '(A)', "2. Loading 'config.hsd'..."
  call hsd_load("config.hsd", config, io_error)
  if (allocated(io_error)) then
     call io_error%print()
     stop 1
  end if

  print '(A)', "3. Validating against schema..."
  ! Use schema_validate_strict to catch unknown fields (typos)
  call schema_validate_strict(schema, config, errors)

  if (allocated(errors) .and. size(errors) > 0) then
     print '(A,I0,A)', "   Found ", size(errors), " validation errors:"
     call print_errors(errors)
     stop 1
  else
     print '(A)', "   Configuration is valid!"
     if (allocated(errors)) deallocate(errors)
  end if

  ! -------------------------------------------------------------------------
  ! 3. Access Data
  ! -------------------------------------------------------------------------
  print *
  print '(A)', "4. Reading configuration..."

  ! Basic gets
  call hsd_get(config, "Server/Port", port)
  call hsd_get(config, "Server/Host", host)  ! Uses deep copy for string

  ! Get with default
  call hsd_get_or(config, "Server/MaxConnections", max_conn, 50)

  ! Get with unit handling (manual way)
  call hsd_get(config, "Server/Timeout", timeout)
  call hsd_get_attrib(config, "Server/Timeout", unit_str)
  if (.not. allocated(unit_str)) unit_str = "s (default)"

  print '(A,I0)', "   Port: ", port
  print '(A,A)',  "   Host: ", host
  print '(A,I0)', "   Max Connections: ", max_conn
  print '(A,F6.2,1X,A)', "   Timeout: ", timeout, unit_str

  ! Enum validation was handled by schema, so we can trust the value
  call hsd_get(config, "Database/Driver", db_driver)
  print '(A,A)',  "   DB Driver: ", db_driver

  ! Cleanup
  print *
  print '(A)', "5. Cleanup"
  call schema_destroy(schema)
  call config%destroy()

contains

  subroutine print_errors(errs)
    type(hsd_error_t), intent(in) :: errs(:)
    integer :: i
    do i = 1, size(errs)
       call errs(i)%print()
    end do
  end subroutine print_errors

end program config_demo
