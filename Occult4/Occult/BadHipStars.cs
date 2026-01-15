namespace Occult
{
	internal class BadHipStars
	{
		private int hip;

		private ulong source_ID;

		private string catID;

		public int Hip
		{
			get
			{
				return hip;
			}
			set
			{
				hip = value;
			}
		}

		public ulong Source_ID
		{
			get
			{
				return source_ID;
			}
			set
			{
				source_ID = value;
			}
		}

		public string CatID
		{
			get
			{
				return catID;
			}
			set
			{
				catID = value;
			}
		}

		public int CompareTo(object other)
		{
			return Hip.CompareTo(((HipStars)other).Hip);
		}

		public override string ToString()
		{
			return Hip + "," + CatID;
		}

		internal void DecodeBadHipLine(string Line)
		{
			string[] array = Line.Split(new char[1] { ',' });
			int.TryParse(array[0], out hip);
			CatID = array[1];
		}
	}
}
