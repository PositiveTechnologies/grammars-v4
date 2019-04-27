# async_stmt: ASYNC (with_stmt | for_stmt);

# async_stmt must be inside async function
async def f():
    # ASYNC with_stmt
    async with open("async_stmt.py") as f:
        pass

    # ASYNC for_stmt
    async for _ in range(5):
        pass
