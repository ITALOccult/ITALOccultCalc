using System;

namespace Occult.Star_Catalogues
{
	public class CatalogConfigEntry
	{
		public static CatalogConfigEntry Empty = new CatalogConfigEntry("*", int.MinValue, null, CatFlags.None, null);

		public readonly string CatalogId;

		public readonly string CatalogName;

		public readonly int ReleaseYear;

		public readonly CatFlags Flags;

		public readonly ReferenceSystem System;

		public readonly string AttName_RA;

		public readonly string AttName_DE;

		public readonly string AttName_PMRA;

		public readonly string AttName_PMDE;

		public readonly string AttName_ERRRA;

		public readonly string AttName_ERRDE;

		public readonly string AttName_ERRPMRA;

		public readonly string AttName_ERRPMDE;

		public readonly string AttName_RUWE;

		public readonly string AttName_EPOCH;

		public readonly string AttName_ID;

		public readonly string FirstEpochAttName;

		public readonly string SecondEpochAttName;

		public readonly string Author;

		public readonly bool DiscrepancyFlagPosition;

		public readonly bool DiscrepancyFlagProperMotion;

		public readonly bool NotInVisierFlag;

		public string CatalogDescriptionLink => $"http://cdsarc.u-strasbg.fr/viz-bin/Cat?{CatalogId}";

		internal CatalogConfigEntry(string catalogId, string author)
		{
			CatalogId = catalogId;
			Author = author;
		}

		internal CatalogConfigEntry(string catalogId, int releaseYear, string parsingParams, CatFlags flags, string catalogName)
		{
			if (catalogId.IndexOf('[') > -1)
			{
				CatalogId = catalogId.Substring(0, catalogId.IndexOf('['));
				string text = catalogId.Substring(catalogId.IndexOf('['));
				DiscrepancyFlagPosition = text.IndexOf('P') > -1;
				DiscrepancyFlagProperMotion = text.IndexOf('M') > -1;
				NotInVisierFlag = text.IndexOf('*') > -1;
			}
			else
			{
				CatalogId = catalogId;
			}
			CatalogName = catalogName;
			ReleaseYear = releaseYear;
			System = ReferenceSystem.ICRS;
			if (parsingParams != null)
			{
				string[] array = parsingParams.Split(new char[1] { ';' }, StringSplitOptions.RemoveEmptyEntries);
				for (int i = 0; i < array.Length; i++)
				{
					string[] array2 = array[i].Split(new char[1] { '=' });
					if ("r" == array2[0])
					{
						AttName_RA = array2[1];
					}
					else if ("d" == array2[0])
					{
						AttName_DE = array2[1];
					}
					else if ("er" == array2[0])
					{
						AttName_ERRRA = array2[1];
					}
					else if ("ed" == array2[0])
					{
						AttName_ERRDE = array2[1];
					}
					else if ("pr" == array2[0])
					{
						AttName_PMRA = array2[1];
					}
					else if ("pd" == array2[0])
					{
						AttName_PMDE = array2[1];
					}
					else if ("epr" == array2[0])
					{
						AttName_ERRPMRA = array2[1];
					}
					else if ("epd" == array2[0])
					{
						AttName_ERRPMDE = array2[1];
					}
					else if ("e" == array2[0])
					{
						if (FirstEpochAttName == null)
						{
							FirstEpochAttName = array2[1];
						}
						else if (SecondEpochAttName == null)
						{
							SecondEpochAttName = array2[1];
						}
						AttName_EPOCH = AttName_EPOCH + array2[1] + "\t";
					}
					else
					{
						if (!("n" == array2[0]))
						{
							throw new ArgumentOutOfRangeException($"Unknown key '{array2[0]}'");
						}
						AttName_ID = AttName_ID + array2[1] + " ";
					}
				}
			}
			Flags = flags;
		}
	}
}
