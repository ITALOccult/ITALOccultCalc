namespace Occult
{
	internal class K2_XZ
	{
		private long ePIC_ID;

		private int xz;

		private short cadence;

		private string field;

		public long EPIC_ID
		{
			get
			{
				return ePIC_ID;
			}
			set
			{
				ePIC_ID = value;
			}
		}

		public short Cadence
		{
			get
			{
				return cadence;
			}
			set
			{
				cadence = value;
			}
		}

		public string Field
		{
			get
			{
				return field;
			}
			set
			{
				field = value;
			}
		}

		public int XZ
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

		public void Read_XZinKepler2_Line(string Line)
		{
			xz = int.Parse(Line.Substring(0, 6));
			cadence = short.Parse(Line.Substring(6, 2));
			if (cadence == 9)
			{
				cadence = 30;
			}
			EPIC_ID = long.Parse(Line.Substring(8, 10));
			field = Line.Substring(18).Trim();
		}
	}
}
