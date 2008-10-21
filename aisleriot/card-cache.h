
#ifndef AISLERIOT_CARD_CACHE_H
#define AISLERIOT_CARD_CACHE_H

#include <glib-object.h>

G_BEGIN_DECLS

#define AISLERIOT_TYPE_CARD_CACHE                                       \
  (aisleriot_card_cache_get_type())
#define AISLERIOT_CARD_CACHE(obj)                                       \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj),                                   \
                               AISLERIOT_TYPE_CARD_CACHE,               \
                               AisleriotCardCache))
#define AISLERIOT_CARD_CACHE_CLASS(klass)                               \
  (G_TYPE_CHECK_CLASS_CAST ((klass),                                    \
                            AISLERIOT_TYPE_CARD_CACHE,                  \
                            AisleriotCardCacheClass))
#define AISLERIOT_IS_CARD_CACHE(obj)                                    \
  (G_TYPE_CHECK_INSTANCE_TYPE ((obj),                                   \
                               AISLERIOT_TYPE_CARD_CACHE))
#define AISLERIOT_IS_CARD_CACHE_CLASS(klass)                            \
  (G_TYPE_CHECK_CLASS_TYPE ((klass),                                    \
                            AISLERIOT_TYPE_CARD_CACHE))
#define AISLERIOT_CARD_CACHE_GET_CLASS(obj)                             \
  (G_TYPE_INSTANCE_GET_CLASS ((obj),                                    \
                              AISLERIOT_TYPE_CARD_CACHE,                \
                              AisleriotCardCacheClass))

typedef struct _AisleriotCardCache        AisleriotCardCache;
typedef struct _AisleriotCardCacheClass   AisleriotCardCacheClass;
typedef struct _AisleriotCardCachePrivate AisleriotCardCachePrivate;

struct _AisleriotCardCacheClass
{
  GObjectClass parent_class;
};

struct _AisleriotCardCache
{
  GObject parent;

  AisleriotCardCachePrivate *priv;
};

GType aisleriot_card_cache_get_type (void) G_GNUC_CONST;

AisleriotCardCache *aisleriot_card_cache_new (GamesCardImages *images);

ClutterActor *aisleriot_card_cache_get_card_texture (AisleriotCardCache *cache,
                                                     Card card,
                                                     gboolean highlighted);
ClutterActor *aisleriot_card_cache_get_card_texture_by_id
                                              (AisleriotCardCache *cache,
                                               guint card_id,
                                               gboolean highlighted);

G_END_DECLS

#endif /* AISLERIOT_CARD_CACHE_H */
