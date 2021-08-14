module type IO = {
  type t('a);

  let return: 'a => t('a);
  let (>>=): (t('a), 'a => t('b)) => t('b);
};
