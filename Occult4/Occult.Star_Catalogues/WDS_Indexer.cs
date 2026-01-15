using System;

namespace Occult.Star_Catalogues
{
	internal class WDS_Indexer : IComparable
	{
		internal static bool SortByName = true;

		private string name;

		private int record;

		private int xz;

		private int xz2;

		private int xz3;

		private int xz4;

		private bool isInXZ;

		private bool nameIsInXZ;

		private double rA;

		private double dec;

		internal string Name
		{
			get
			{
				return name;
			}
			set
			{
				name = value;
			}
		}

		internal int Record
		{
			get
			{
				return record;
			}
			set
			{
				record = value;
			}
		}

		internal bool IsInXZ
		{
			get
			{
				return isInXZ;
			}
			set
			{
				isInXZ = value;
			}
		}

		internal bool NameIsInXZ
		{
			get
			{
				return nameIsInXZ;
			}
			set
			{
				nameIsInXZ = value;
			}
		}

		internal double RA
		{
			get
			{
				return rA;
			}
			set
			{
				rA = value;
			}
		}

		internal double Dec
		{
			get
			{
				return dec;
			}
			set
			{
				dec = value;
			}
		}

		internal int XZ
		{
			get
			{
				return xz;
			}
			set
			{
				xz = value;
			}
		}

		internal int XZ2
		{
			get
			{
				return xz2;
			}
			set
			{
				xz2 = value;
			}
		}

		internal int XZ3
		{
			get
			{
				return xz3;
			}
			set
			{
				xz3 = value;
			}
		}

		internal int XZ4
		{
			get
			{
				return xz4;
			}
			set
			{
				xz4 = value;
			}
		}

		public int CompareTo(object other)
		{
			if (SortByName)
			{
				return Name.CompareTo(((WDS_Indexer)other).Name);
			}
			return RA.CompareTo(((WDS_Indexer)other).RA);
		}

		public override string ToString()
		{
			return name + string.Format("  record {0,1}", record);
		}
	}
}
