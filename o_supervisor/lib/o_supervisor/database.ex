defmodule OSupervisor.Database do
  @moduledoc """
  PostgreSQL database operations for Project O.

  Provides structured storage for agents, messages, and memory.
  Extends MemoryVault with relational database capabilities.
  """

  use GenServer
  require Logger

  defstruct [:conn, :config]

  # Client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  # Agent Operations

  def create_agent(params) do
    GenServer.call(__MODULE__, {:create_agent, params})
  end

  def get_agent(agent_id) do
    GenServer.call(__MODULE__, {:get_agent, agent_id})
  end

  def update_agent(agent_id, params) do
    GenServer.call(__MODULE__, {:update_agent, agent_id, params})
  end

  def delete_agent(agent_id) do
    GenServer.call(__MODULE__, {:delete_agent, agent_id})
  end

  def list_agents(opts \\ []) do
    GenServer.call(__MODULE__, {:list_agents, opts})
  end

  # Message Operations

  def create_message(agent_id, params) do
    GenServer.call(__MODULE__, {:create_message, agent_id, params})
  end

  def get_messages(agent_id, opts \\ []) do
    GenServer.call(__MODULE__, {:get_messages, agent_id, opts})
  end

  def delete_message(message_id) do
    GenServer.call(__MODULE__, {:delete_message, message_id})
  end

  # Memory Block Operations

  def create_memory_block(agent_id, params) do
    GenServer.call(__MODULE__, {:create_memory_block, agent_id, params})
  end

  def get_memory_blocks(agent_id) do
    GenServer.call(__MODULE__, {:get_memory_blocks, agent_id})
  end

  def update_memory_block(agent_id, label, value) do
    GenServer.call(__MODULE__, {:update_memory_block, agent_id, label, value})
  end

  # Archival Memory Operations

  def insert_archival_memory(agent_id, params) do
    GenServer.call(__MODULE__, {:insert_archival_memory, agent_id, params})
  end

  def search_archival_memory(agent_id, query, opts \\ []) do
    GenServer.call(__MODULE__, {:search_archival_memory, agent_id, query, opts})
  end

  # Statistics Operations

  def get_agent_statistics(agent_id) do
    GenServer.call(__MODULE__, {:get_agent_statistics, agent_id})
  end

  def update_statistics(agent_id, updates) do
    GenServer.call(__MODULE__, {:update_statistics, agent_id, updates})
  end

  # Server Callbacks

  @impl true
  def init(opts) do
    config = %{
      hostname: Keyword.get(opts, :hostname, "localhost"),
      port: Keyword.get(opts, :port, 5432),
      database: Keyword.get(opts, :database, "project_o"),
      username: Keyword.get(opts, :username, "postgres"),
      password: Keyword.get(opts, :password, ""),
      pool_size: Keyword.get(opts, :pool_size, 10)
    }

    case Postgrex.start_link(config) do
      {:ok, conn} ->
        Logger.info("Database connected: #{config.database}@#{config.hostname}")
        {:ok, %__MODULE__{conn: conn, config: config}}

      {:error, reason} ->
        Logger.error("Database connection failed: #{inspect(reason)}")
        {:stop, reason}
    end
  end

  # Agent Operations Implementation

  @impl true
  def handle_call({:create_agent, params}, _from, state) do
    query = """
    INSERT INTO agents (name, llm_provider, llm_model, llm_temperature,
                       llm_max_tokens, system_prompt, core_memory_enabled,
                       archival_memory_enabled, max_archival_entries, enabled_tools)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
    RETURNING id, name, llm_provider, llm_model, created_at, updated_at
    """

    params_list = [
      Map.get(params, :name),
      Map.get(params, :llm_provider, "openai"),
      Map.get(params, :llm_model, "gpt-4"),
      Map.get(params, :llm_temperature, 0.7),
      Map.get(params, :llm_max_tokens, 4096),
      Map.get(params, :system_prompt, "You are a helpful assistant."),
      Map.get(params, :core_memory_enabled, true),
      Map.get(params, :archival_memory_enabled, true),
      Map.get(params, :max_archival_entries, 10000),
      Map.get(params, :enabled_tools, ["send_message", "conversation_search"])
    ]

    case Postgrex.query(state.conn, query, params_list) do
      {:ok, %{rows: [row]}} ->
        agent = row_to_agent(row)

        # Initialize memory blocks
        initialize_memory_blocks(state.conn, agent.id)

        # Initialize statistics
        initialize_statistics(state.conn, agent.id)

        {:reply, {:ok, agent}, state}

      {:error, reason} ->
        Logger.error("Failed to create agent: #{inspect(reason)}")
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:get_agent, agent_id}, _from, state) do
    query = """
    SELECT id, name, llm_provider, llm_model, llm_temperature, llm_max_tokens,
           system_prompt, core_memory_enabled, archival_memory_enabled,
           max_archival_entries, enabled_tools, created_at, updated_at
    FROM agents
    WHERE id = $1 AND deleted_at IS NULL
    """

    case Postgrex.query(state.conn, query, [agent_id]) do
      {:ok, %{rows: [row]}} ->
        {:reply, {:ok, row_to_agent(row)}, state}

      {:ok, %{rows: []}} ->
        {:reply, {:error, :not_found}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:update_agent, agent_id, params}, _from, state) do
    # Build dynamic UPDATE query based on provided params
    {set_clauses, values} = build_update_clauses(params, 2)

    query = """
    UPDATE agents
    SET #{Enum.join(set_clauses, ", ")}, updated_at = CURRENT_TIMESTAMP
    WHERE id = $1 AND deleted_at IS NULL
    RETURNING id, name, llm_provider, llm_model, created_at, updated_at
    """

    case Postgrex.query(state.conn, query, [agent_id | values]) do
      {:ok, %{rows: [row]}} ->
        {:reply, {:ok, row_to_agent(row)}, state}

      {:ok, %{rows: []}} ->
        {:reply, {:error, :not_found}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:delete_agent, agent_id}, _from, state) do
    # Soft delete
    query = """
    UPDATE agents
    SET deleted_at = CURRENT_TIMESTAMP
    WHERE id = $1 AND deleted_at IS NULL
    """

    case Postgrex.query(state.conn, query, [agent_id]) do
      {:ok, _} ->
        {:reply, :ok, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:list_agents, opts}, _from, state) do
    limit = Keyword.get(opts, :limit, 10)
    offset = Keyword.get(opts, :offset, 0)

    query = """
    SELECT id, name, llm_provider, llm_model, created_at, updated_at
    FROM agents
    WHERE deleted_at IS NULL
    ORDER BY created_at DESC
    LIMIT $1 OFFSET $2
    """

    case Postgrex.query(state.conn, query, [limit, offset]) do
      {:ok, %{rows: rows}} ->
        agents = Enum.map(rows, &row_to_agent/1)
        {:reply, {:ok, agents}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  # Message Operations Implementation

  @impl true
  def handle_call({:create_message, agent_id, params}, _from, state) do
    query = """
    INSERT INTO messages (agent_id, role, content, tool_calls, tool_call_id,
                         tool_name, prompt_tokens, completion_tokens, total_tokens)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
    RETURNING id, agent_id, role, content, created_at
    """

    params_list = [
      agent_id,
      Map.get(params, :role),
      Map.get(params, :content),
      Map.get(params, :tool_calls) |> encode_json(),
      Map.get(params, :tool_call_id),
      Map.get(params, :tool_name),
      Map.get(params, :prompt_tokens),
      Map.get(params, :completion_tokens),
      Map.get(params, :total_tokens)
    ]

    case Postgrex.query(state.conn, query, params_list) do
      {:ok, %{rows: [row]}} ->
        message = row_to_message(row)

        # Update statistics
        update_message_statistics(state.conn, agent_id, params)

        {:reply, {:ok, message}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:get_messages, agent_id, opts}, _from, state) do
    limit = Keyword.get(opts, :limit, 50)
    offset = Keyword.get(opts, :offset, 0)
    role = Keyword.get(opts, :role)

    query = if role do
      """
      SELECT id, agent_id, role, content, tool_calls, created_at, total_tokens
      FROM messages
      WHERE agent_id = $1 AND role = $2
      ORDER BY created_at DESC
      LIMIT $3 OFFSET $4
      """
    else
      """
      SELECT id, agent_id, role, content, tool_calls, created_at, total_tokens
      FROM messages
      WHERE agent_id = $1
      ORDER BY created_at DESC
      LIMIT $2 OFFSET $3
      """
    end

    params = if role, do: [agent_id, role, limit, offset], else: [agent_id, limit, offset]

    case Postgrex.query(state.conn, query, params) do
      {:ok, %{rows: rows}} ->
        messages = Enum.map(rows, &row_to_message/1)
        {:reply, {:ok, messages}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:delete_message, message_id}, _from, state) do
    query = "DELETE FROM messages WHERE id = $1"

    case Postgrex.query(state.conn, query, [message_id]) do
      {:ok, _} ->
        {:reply, :ok, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  # Memory Block Operations Implementation

  @impl true
  def handle_call({:create_memory_block, agent_id, params}, _from, state) do
    query = """
    INSERT INTO memory_blocks (agent_id, label, value, is_template, is_readonly)
    VALUES ($1, $2, $3, $4, $5)
    ON CONFLICT (agent_id, label) DO UPDATE SET value = EXCLUDED.value
    RETURNING id, agent_id, label, value, created_at, updated_at
    """

    params_list = [
      agent_id,
      Map.get(params, :label),
      Map.get(params, :value),
      Map.get(params, :is_template, false),
      Map.get(params, :is_readonly, false)
    ]

    case Postgrex.query(state.conn, query, params_list) do
      {:ok, %{rows: [row]}} ->
        {:reply, {:ok, row_to_memory_block(row)}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:get_memory_blocks, agent_id}, _from, state) do
    query = """
    SELECT id, agent_id, label, value, is_template, is_readonly, created_at, updated_at
    FROM memory_blocks
    WHERE agent_id = $1
    ORDER BY label
    """

    case Postgrex.query(state.conn, query, [agent_id]) do
      {:ok, %{rows: rows}} ->
        blocks = Enum.map(rows, &row_to_memory_block/1)
        {:reply, {:ok, blocks}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:update_memory_block, agent_id, label, value}, _from, state) do
    query = """
    UPDATE memory_blocks
    SET value = $3, updated_at = CURRENT_TIMESTAMP
    WHERE agent_id = $1 AND label = $2 AND is_readonly = false
    RETURNING id, agent_id, label, value, updated_at
    """

    case Postgrex.query(state.conn, query, [agent_id, label, value]) do
      {:ok, %{rows: [row]}} ->
        {:reply, {:ok, row_to_memory_block(row)}, state}

      {:ok, %{rows: []}} ->
        {:reply, {:error, :not_found_or_readonly}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  # Archival Memory Operations Implementation

  @impl true
  def handle_call({:insert_archival_memory, agent_id, params}, _from, state) do
    query = """
    INSERT INTO archival_memory (agent_id, content, embedding, importance, tags)
    VALUES ($1, $2, $3, $4, $5)
    RETURNING id, agent_id, content, importance, tags, created_at
    """

    params_list = [
      agent_id,
      Map.get(params, :content),
      Map.get(params, :embedding),
      Map.get(params, :importance, 0.5),
      Map.get(params, :tags, [])
    ]

    case Postgrex.query(state.conn, query, params_list) do
      {:ok, %{rows: [row]}} ->
        {:reply, {:ok, row_to_archival_memory(row)}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:search_archival_memory, agent_id, query_text, opts}, _from, state) do
    limit = Keyword.get(opts, :limit, 10)
    search_type = Keyword.get(opts, :search_type, :text)

    query = case search_type do
      :text ->
        """
        SELECT id, agent_id, content, importance, tags, created_at
        FROM archival_memory
        WHERE agent_id = $1 AND content ILIKE $2
        ORDER BY importance DESC, created_at DESC
        LIMIT $3
        """

      :semantic ->
        # TODO: Implement vector similarity search
        # Requires embedding vector for query
        """
        SELECT id, agent_id, content, importance, tags, created_at
        FROM archival_memory
        WHERE agent_id = $1
        ORDER BY embedding <=> $2
        LIMIT $3
        """
    end

    search_param = if search_type == :text, do: "%#{query_text}%", else: query_text

    case Postgrex.query(state.conn, query, [agent_id, search_param, limit]) do
      {:ok, %{rows: rows}} ->
        results = Enum.map(rows, &row_to_archival_memory/1)
        {:reply, {:ok, results}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  # Statistics Operations Implementation

  @impl true
  def handle_call({:get_agent_statistics, agent_id}, _from, state) do
    query = """
    SELECT agent_id, total_messages, user_messages, assistant_messages,
           total_tokens, prompt_tokens, completion_tokens,
           archival_memory_count, recall_memory_count,
           evolution_attempts, successful_evolutions,
           first_message_at, last_message_at, last_evolution_at
    FROM agent_statistics
    WHERE agent_id = $1
    """

    case Postgrex.query(state.conn, query, [agent_id]) do
      {:ok, %{rows: [row]}} ->
        {:reply, {:ok, row_to_statistics(row)}, state}

      {:ok, %{rows: []}} ->
        {:reply, {:error, :not_found}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:update_statistics, agent_id, updates}, _from, state) do
    {set_clauses, values} = build_update_clauses(updates, 2)

    query = """
    UPDATE agent_statistics
    SET #{Enum.join(set_clauses, ", ")}, updated_at = CURRENT_TIMESTAMP
    WHERE agent_id = $1
    """

    case Postgrex.query(state.conn, query, [agent_id | values]) do
      {:ok, _} ->
        {:reply, :ok, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  # Helper Functions

  defp initialize_memory_blocks(conn, agent_id) do
    query = """
    INSERT INTO memory_blocks (agent_id, label, value)
    VALUES
      ($1, 'persona', 'You are a helpful AI assistant.'),
      ($1, 'human', 'User information will be stored here.')
    """

    Postgrex.query(conn, query, [agent_id])
  end

  defp initialize_statistics(conn, agent_id) do
    query = "INSERT INTO agent_statistics (agent_id) VALUES ($1)"
    Postgrex.query(conn, query, [agent_id])
  end

  defp update_message_statistics(conn, agent_id, params) do
    role = Map.get(params, :role)
    tokens = Map.get(params, :total_tokens, 0)

    role_field = case role do
      "user" -> "user_messages"
      "assistant" -> "assistant_messages"
      _ -> nil
    end

    if role_field do
      query = """
      UPDATE agent_statistics
      SET total_messages = total_messages + 1,
          #{role_field} = #{role_field} + 1,
          total_tokens = total_tokens + $2,
          last_message_at = CURRENT_TIMESTAMP,
          first_message_at = COALESCE(first_message_at, CURRENT_TIMESTAMP)
      WHERE agent_id = $1
      """

      Postgrex.query(conn, query, [agent_id, tokens])
    end
  end

  defp build_update_clauses(params, start_index) do
    params
    |> Enum.with_index(start_index)
    |> Enum.map(fn {{key, _value}, index} ->
      {"#{key} = $#{index}", index}
    end)
    |> Enum.unzip()
    |> then(fn {clauses, _indices} ->
      values = Map.values(params)
      {clauses, values}
    end)
  end

  defp encode_json(nil), do: nil
  defp encode_json(data), do: Jason.encode!(data)

  defp row_to_agent(row) do
    [id, name, provider, model, created_at, updated_at | _] = row
    %{
      id: id,
      name: name,
      llm_provider: provider,
      llm_model: model,
      created_at: created_at,
      updated_at: updated_at
    }
  end

  defp row_to_message(row) do
    [id, agent_id, role, content, tool_calls, created_at, total_tokens | _] = row
    %{
      id: id,
      agent_id: agent_id,
      role: role,
      content: content,
      tool_calls: tool_calls,
      created_at: created_at,
      total_tokens: total_tokens
    }
  end

  defp row_to_memory_block(row) do
    [id, agent_id, label, value, is_template, is_readonly, created_at, updated_at | _] = row
    %{
      id: id,
      agent_id: agent_id,
      label: label,
      value: value,
      is_template: is_template,
      is_readonly: is_readonly,
      created_at: created_at,
      updated_at: updated_at
    }
  end

  defp row_to_archival_memory(row) do
    [id, agent_id, content, importance, tags, created_at | _] = row
    %{
      id: id,
      agent_id: agent_id,
      content: content,
      importance: importance,
      tags: tags,
      created_at: created_at
    }
  end

  defp row_to_statistics(row) do
    [agent_id, total_messages, user_messages, assistant_messages,
     total_tokens, prompt_tokens, completion_tokens,
     archival_count, recall_count, evolution_attempts, successful_evolutions,
     first_message_at, last_message_at, last_evolution_at | _] = row

    %{
      agent_id: agent_id,
      total_messages: total_messages,
      user_messages: user_messages,
      assistant_messages: assistant_messages,
      total_tokens: total_tokens,
      prompt_tokens: prompt_tokens,
      completion_tokens: completion_tokens,
      archival_memory_count: archival_count,
      recall_memory_count: recall_count,
      evolution_attempts: evolution_attempts,
      successful_evolutions: successful_evolutions,
      first_message_at: first_message_at,
      last_message_at: last_message_at,
      last_evolution_at: last_evolution_at
    }
  end

  @impl true
  def terminate(_reason, state) do
    if state.conn, do: GenServer.stop(state.conn)
    :ok
  end
end
