#include "erl_nif.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
}

static ERL_NIF_TERM get_fd_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int sock;
    struct sockaddr_in name;
    ERL_NIF_TERM ok, error, result;

    if ((sock=socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        error = enif_make_atom(env, "error");
        result = enif_make_atom(env, "socket");
        return enif_make_tuple2(env, error, result);
    }

    name.sin_family = AF_INET;
    name.sin_port = htons(2021);
    name.sin_addr.s_addr = htonl (INADDR_ANY);
    if (bind (sock, (struct sockaddr *) &name, sizeof (name)) < 0) {
        error = enif_make_atom(env, "error");
        result = enif_make_atom(env, "bind");
        return enif_make_tuple2(env, error, result);
    }

    ok = enif_make_atom(env, "ok");
    result = enif_make_int(env, sock);
    return enif_make_tuple2(env, ok, result);
}

static ERL_NIF_TERM close_fd_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int sock;

    if (argc!=1) {
        return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[0], &sock)) {
        return enif_make_badarg(env);
    }
    close(sock);
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"get_fd", 0, get_fd_nif},
    {"close_fd", 1, close_fd_nif}
};

ERL_NIF_INIT(fd_nif, nif_funcs, load, reload, upgrade, unload)
